####################################################################################################
####################################################################################################
################################ Functions adapted from initial jupyter notebook  V5.2
################################ for NASA data anlaysis
####################################################################################################
####################################################################################################

from sqlalchemy import (UniqueConstraint,MetaData,Table,Column,Integer,Numeric,Float,Boolean,String,DateTime,ForeignKey,create_engine,select,update,delete,insert)
from sqlalchemy_utils.functions import create_database
from sqlalchemy import inspect
from sqlalchemy import func
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
import os
import pytz
from pytz import timezone
import json
from statsmodels.tsa.ar_model import AR, AutoReg
from statsmodels.tsa.stattools import acf
import sys
from pathlib import Path

####################################################################################################
################################ Load task list info
####################################################################################################

def get_task_list(task_lists_and_categories, data_dir):
    '''This pulls in and formats all task lists'''
    # reads in from tracking file
    t_l_df = pd.read_excel(os.path.join(data_dir,task_lists_and_categories), sheet_name = 'task_lists')
    tasks_mission_dict = dict(zip(t_l_df.mission,t_l_df.task_list))
    # reads in and combines all of the task list documents
    dfs = []
    for mission, file in tasks_mission_dict.items():
        df = pd.read_excel(os.path.join(data_dir,'HERA_tasklists/',file),header=0,index_col=None,converters={'Duratrion':str()})
        df['Team'] = mission
        dfs.append(df)
    tasks_df = pd.concat(dfs,sort=False,ignore_index=True)

    #Creates timestamps and standardizes timezones
    tasks_df['Start Time'] = pd.to_datetime(tasks_df['Start Time'])
    # set timezones... task list times are in central
    tasks_df['Start Time'] = tasks_df['Start Time'].dt.tz_localize(pytz.timezone('US/Central'))
    # convert to UTC... E4 timestamps are stored in utc
    tasks_df['Start Time'] = tasks_df['Start Time'].dt.tz_convert(pytz.timezone('UTC'))

    # create end time from 'Duration' column
    tasks_df['Duration'] = tasks_df['Duration'].astype('str')
    tasks_df['End_offset'] = tasks_df['Duration'].apply(lambda x: timedelta(hours=int(x.split(':')[0]), minutes = int(x.split(':')[1])))
    tasks_df['Stop Time'] = tasks_df['Start Time'] + tasks_df['End_offset']

    # parses mission day codes... need to expand for pre-mission coding syntax ultimately (MD-)
    #tasks_df['Mission_day'] = tasks_df['MD'].str.split(' ')[1].astype('int')
    tasks_df['Mission_day'] = tasks_df['MD'].str.replace('MD','')
    tasks_df['Mission_day'] = tasks_df['Mission_day'].str.strip()
    tasks_df['Mission_day'] = tasks_df['Mission_day'].astype('int')

    #Creates column for task categories and codes based on specific activity
    t_c_df = pd.read_excel(os.path.join(data_dir,task_lists_and_categories), sheet_name = 'task_categories')
    task_categories = dict(zip(t_c_df.task,t_c_df.category))
    tasks_df['Task_category'] = tasks_df['Activity Name'].replace(task_categories)
    categories = [x for x in set(list(task_categories.values())) if x != 'None' ]
    tasks_df = tasks_df[tasks_df.Task_category.isin(categories)]
    tasks_df.dropna(axis=0,how='any',subset=['Task_category'],inplace=True)
    tasks_df.reset_index(drop=True,inplace=True)
    tasks_df['Task_num'] = tasks_df.index.values

    return(tasks_df)

####################################################################################################
################################ Functions for E4 data processsing
####################################################################################################


'''Add import functions'''

### Creates energy metric for ACC data
def create_ACC_energy_metric(E4_data):
    # convert x, y , z to energy metric
    # consider 10.1371/journal.pone.0160644
    dimensions = ['x','y','z']
    for dimension in dimensions:
        E4_data[dimension] = E4_data[dimension].apply(lambda x: np.square(x))
    E4_data['energy'] = E4_data[dimensions].sum(axis=1)
    E4_data['energy'] = E4_data['energy']**(1/2)
    E4_data.drop(columns=dimensions,inplace=True)
    return(E4_data)

### Calculates matrix of synch coefficients

def get_sync_coef(E4_tab, Role_E4_dict, sampling_freq, offset, use_residuals, corr_method):
    ### rename columns as roles instead of device IDs
    E4_tab.columns.name = None
    Role_to_cols = {v: k for k, v in Role_E4_dict.items()}
    E4_tab.rename(columns=Role_to_cols,inplace=True)
    working_roles = list(E4_tab.columns.values) # for only processing roles present in the data
    ### Sets up df to calculate and store data for given task
    Sync_Coefs = pd.DataFrame(index=working_roles,columns=working_roles,data=None)

    ### Creates Table 1 in Guastello and Perisini
    for from_role in working_roles: # from roles are rows of sync_coef matrix from guastello
        ### calculate and store autocorrelation
        Sync_Coefs.loc[from_role,from_role] = acf(E4_tab[from_role],fft = True, adjusted=True, nlags=offset)[offset]
        to_roles = [role for role in working_roles if role != from_role] # gets all other roles
        for to_role in to_roles:
            E4_temp = E4_tab.copy()

            # gets residuals from acf (controls for autocorrelation)... maybe better way to do this???
            E4_temp.dropna(axis=0,inplace=True,how='any')
            E4_temp = E4_temp.asfreq(freq=sampling_freq)
            if use_residuals:
                to_residuals =  AutoReg(E4_temp[to_role], lags = [offset]).fit().resid
                to_residuals = pd.DataFrame({'TimeStamp':to_residuals.index, to_role:to_residuals.values})
                to_residuals.set_index('TimeStamp', inplace = True)
                E4_temp.drop(columns=to_role, inplace = True)
                E4_temp = E4_temp.merge(to_residuals, on = 'TimeStamp', how = 'outer')
            else: pass

            E4_temp[to_role] = E4_temp[to_role].shift(periods=(offset*-1),freq=sampling_freq)
            E4_temp.dropna(axis=0,inplace=True,how='any')
            '''
            HEY! below where it says .corr(method=corr_method) is where you can plug in any python function that takes two arrays
            and returns a float. Right now it's set above and does a pearson corrleation. There may be better ways to do this all around,
            but if you believe the docs the .corr method will take any function you define.
            '''
            coef_matrix = E4_temp[[from_role,to_role]].corr(method=corr_method) # RIGHT HERE!!!
            Sync_Coefs.loc[from_role,to_role] = coef_matrix.loc[from_role,to_role]
    return(Sync_Coefs, working_roles)

def update_sync_metrics(Sync_df, Sync_Coefs, i, row, working_roles, measure):
    Se = 'Se_'+measure
    Sync_df.loc[i,'Task_num'] = row['Task_num']
    print(row['Task_num'])
    highest_empath = ()
    Sync_Coefs_sq = np.square(Sync_Coefs) # added to
    for role in working_roles:
        Sync_df.loc[i,str(role+'_AR_'+measure)] = Sync_Coefs_sq.loc[role,role]
        e_score = Sync_Coefs_sq[role].sum() #empath scores
        Sync_df.loc[i,str(role+'_Empath_'+measure)] = e_score
        if not highest_empath:
            highest_empath = (role,e_score)
        elif highest_empath[1] < e_score:
            highest_empath = (role,e_score)
        else: pass
        Sync_df.loc[i,str(role+'_Driver_'+measure)] = Sync_Coefs_sq.loc[role,working_roles].sum(axis=0) #driver scores

    # saves Se score
    if highest_empath:
        empath = highest_empath[0]
        V_prime = Sync_Coefs_sq[empath].copy()
        V_prime.drop(index=empath,inplace=True)
        M = Sync_Coefs_sq.drop(columns=empath)
        M.drop(index=empath,inplace=True)
        if not M.isnull().values.any(): #skips if there is missing info.. need to figure out why it would get here and be empty
            M1 = M.astype('float',copy=True)
            M_inv = pd.DataFrame(data = np.linalg.pinv(M1.values),columns=M1.columns,index=M1.index)
            Q = M_inv.dot(V_prime)
            Sync_df.loc[i,Se] = V_prime.dot(Q)
            print(V_prime.dot(Q))
        else:
            Sync_df.loc[i,Se] = 99
            print('No getting to Se calc..')
            print(M)
    else: print('no highest empath?')
    return(Sync_df)

### Funciton to connect to appropriate db (workaround for not being able to share large db files on onedrive)
def db_connection(db_path,mission,MD):
    #from pathlib import Path
    connection = None
    p = 'sqlite:///'+str(Path(db_path,str(mission),'MD_'+str(MD)+'.db').resolve())
    engine = create_engine(p)
    metadata = MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
    return connection, metadata

### Pulls all data for given task and roles
def get_E4_data(E4_ids, measure, sampling_freq, db_path, row):
    E4_tab = pd.DataFrame(data = None) # One dataframe for all of the E4 data for specific task
    all_E4s_inData = True # flag used to stop synch calculations if there is missing E4 data for a task
    ### this for loop builds dataframe with timestamp as index, device id's as column names, and measure as values
    for E4 in E4_ids:
        E4_data = pd.DataFrame(data = None) # df for specific role for specific task
        try:
            connection, metadata = db_connection(db_path,row.Team,row.Mission_day)
            t_name = str('Table_'+E4+'_'+measure)
            t = metadata.tables[t_name]
            s = select([t]).where((t.c.TimeStamp >= row['Start Time']) & (t.c.TimeStamp <= row['Stop Time']))
            rp = connection.execute(s)
            E4_data = pd.DataFrame(rp.fetchall())
        except:
            print('Problem with DB for '+E4+'_'+measure+' in '+str(row.Team)+' on '+str(row.Mission_day))
        ### checks for no data in df and flags fact that there's missing E4 data to stop synchrony calcs below
        if E4_data.empty:
            print('No data in DB...')
            all_E4s_inData = False # will stop further synch calculations below
        else:
            E4_data.columns = rp.keys() # assigns column names for db
            # Set TZ for timestamps
            E4_data.TimeStamp = pd.to_datetime(E4_data.TimeStamp)
            E4_data.TimeStamp = E4_data.TimeStamp.dt.tz_localize(pytz.timezone('UTC')) # E4 timestamps are stored in utc
            E4_data.set_index('TimeStamp', inplace = True)

            ### Creates energy metric for ACC data
            if measure == 'ACC':
                E4_data = create_ACC_energy_metric(E4_data)
            else: pass

            ### resamples to the set sampling frequency (in config file)
            E4_data = E4_data.resample(sampling_freq).mean()

            ### renames data column to device id
            E4_data.columns = [E4]
            ### adds E4_data for one role to the overall E4_tab dataframe
            if E4_tab.empty:
                E4_tab = E4_data.copy()
            else:
                E4_tab = E4_tab.merge(E4_data, on = 'TimeStamp', how = 'outer')
    return(E4_tab, all_E4s_inData)

def get_E4_synchronies(tasks_df, offset, roles, measures, sampling_freq, corr_method, use_residuals, missing_E4_flag, NA_E4_flag, db_path, of_path, save_csv):
    ### Makes sure things are typed correctly (they are switching to float being pased to R and back)
    tasks_df['Mission_day'] = tasks_df['Mission_day'].astype('int')
    tasks_df['Task_num'] = tasks_df['Task_num'].astype('int')
    # Set up dataframe to store results; creates one dataframe for all loops
    cols = ['Task_num']
    for measure in measures: cols.append('Se_'+measure)
    for role in roles:
        cols.append(role+'_Driver_'+measure)
        cols.append(role+'_Empath_'+measure)
        cols.append(role+'_AR_'+measure)
    Sync_df = pd.DataFrame(columns=cols,data=None,index=tasks_df.index)
    ### for each measure, calculate synchronices for each task in the task list
    for measure in measures:
        ### Iterate through each task in the task dataframe, create Table 1 measures from Guastello and Peressini:
        for i, row in tasks_df.iterrows():
            E4_ids =  [x for x in row[roles].values if str(x) != 'nan'] # Drops any roles that have no E4 ID
            E4_ids = [x for x in E4_ids if str(x) != NA_E4_flag] # Drops any roles with fallged issues with E4 data
            Role_E4_dict = row[roles].to_dict() # Creates dict for relabeling roles and device IDs
            # runs for all tasks with a dyad or greater with no missing E4 data
            if (len(E4_ids) <= 1):
                print('Task List: Too few E4s..')
                pass
            elif (any(missing_E4_flag in x for x in E4_ids)):
                print('Task List: Missing E4 data...')
                pass
            elif (any(';' in x for x in E4_ids)) or (any('(' in x for x in E4_ids)) or (any(':' in x for x in E4_ids)): #need to expand this to make it measure specific for NEDA or NBVP
                print('Task List: Incomplete E4 data...')
                pass
            else:
                E4_tab, all_E4s_inData = get_E4_data(E4_ids = E4_ids, measure = measure,
                                                    sampling_freq = sampling_freq, db_path = db_path, row = row)
                if all_E4s_inData:
                    ### Calculates time lagged correlation matrix
                    Sync_Coefs, working_roles = get_sync_coef(E4_tab = E4_tab, Role_E4_dict = Role_E4_dict,
                                                                sampling_freq = sampling_freq, offset = offset,
                                                                use_residuals = use_residuals, corr_method = corr_method)
                    # Calculates and saves AR, driver and empath scores as well as overall Se score
                    Sync_df = update_sync_metrics(Sync_df = Sync_df, Sync_Coefs = Sync_Coefs, i = i, row = row,
                                                    working_roles = working_roles, measure = measure)
                else: pass
    if save_csv:
        m = '_'.join(measures)
        out_file = 'Sync_df_{}-{}offset_{}residuals_{}corrMethod_runOn_{}.csv'.format(m,str(offset),str(use_residuals),corr_method,datetime.date(datetime.now()))
        Sync_df.to_csv(os.path.join(of_path,out_file),index=False)
    return(Sync_df)

def reshape_sync_data(sync_df, tasks_df, roles, metrics, measures, data_dir, of_path, save_csv):

    sync_df.dropna(subset=['Task_num'],inplace=True)
    sync_df['Task_num'] = sync_df['Task_num'].astype('int')
    sync_df = sync_df.merge(tasks_df,on='Task_num',how='outer')

    value_vars = []
    for role in roles:
        for metric in metrics:
            for measure in measures:
                value_vars.append('_'.join([role,metric,measure]))

    Sync_df_long = pd.melt(sync_df,
                id_vars=['Task_num','Mission_day','Team','Start Time','Activity Name','Crew Members','Task_category'],
                #value_vars=['MS1_Driver_HR','MS1_Empath_HR'],
                value_vars=value_vars,
                value_name='Sync_Coef'
               )
    Sync_df_long['Role'] = Sync_df_long.variable.apply(lambda s: s.split("_")[0])
    Sync_df_long['Sync_Type'] = Sync_df_long.variable.apply(lambda s: s.split("_")[1])
    Sync_df_long['Physio_meas'] = Sync_df_long.variable.apply(lambda s: s.split("_")[2])
    #Sync_df_long['Day'] = Sync_df_long.MD.apply(lambda s: s.split(" ")[1])# THIS WON"T WORK WHEN ADDING PMD

    if save_csv:
        out_file = 'Sync_df_LONG_{}-{}offset_{}residuals_{}corrMethod_runOn_{}.csv'.format(m,str(offset),str(use_residuals),corr_method,datetime.date(datetime.now()))
        Sync_df_long.to_csv(os.path.join(of_path,out_file),index=False)

    # # Adds participant ID
    roles_IDs_df = pd.read_excel(os.path.join(data_dir,'HERA_Roles_to_IDs_C5.xlsx'),header=0,index_col=False)
    mapper = roles_IDs_df.pivot(index='Mission',columns='Role',values='ID').to_dict(orient='index')
    Sync_df_long['Part_ID'] = Sync_df_long.apply(lambda x: mapper[x.Team][x.Role],axis=1)

    # # resturcture so there is a column for each measure[EDA, HR etc].metric[AR, empath, driver].task
    Sync_df_long['temp_i'] = Sync_df_long.apply(lambda x: str(x.Task_num)+'_'+str(x.Team)+'_'+str(x.Part_ID)+'_'+str(x.Mission_day),axis=1)
    Sync_df_long['new_cols'] = Sync_df_long.apply(lambda x: str(x.Physio_meas)+'.'+str(x.Sync_Type)+'.'+str(x.Task_category), axis=1)
    z = Sync_df_long.pivot(index='temp_i',columns='new_cols',values='Sync_Coef')
    z.reset_index(inplace=True)
    z.index.name = None
    z[[col for col in z.columns if col != 'temp_i']] = z[[col for col in z.columns if col != 'temp_i']].astype('float')
    z['Task_num'] = z['temp_i'].apply(lambda s: s.split("_")[0])
    z['Team'] = z.temp_i.apply(lambda s: s.split("_")[1])
    z['Part_ID'] = z.temp_i.apply(lambda s: s.split("_")[2])
    z['Mission_day'] = z.temp_i.apply(lambda s: s.split("_")[3])

    # # sum across tasks during each day
    z = z.groupby(['Team','Part_ID','Mission_day']).mean()
    z.reset_index(inplace=True)
    z.index.name = None

    # # create an average for each day across task categories
    for meas_met in [x+'.'+y for x in measures for y in list(Sync_df_long.Sync_Type.unique())]:
        cols =[meas_met+'.'+task for task in list(Sync_df_long.Task_category.unique())]
        z[str(meas_met+'.Avg')] = z[cols].mean(skipna=True,axis=1)

    return(z)


####################################################################################################
################################ Functions for HERA Survey Data
####################################################################################################
