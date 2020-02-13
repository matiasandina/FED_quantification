# -*- coding: utf-8 -*-
"""
Created on Mon Sep 17 13:52:10 2018

@author: i
"""

from tkinter import *
from tkinter import filedialog
from tkinter import messagebox
import datetime
from time import strftime
import os
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as md
from matplotlib.ticker import MaxNLocator
import matplotlib.offsetbox as offsetbox

# options for drop down menus in dialog window
hours_months = [1,2,3,4,5,6,7,8,9,10,11,12]
am_pm = ["am","pm"]
days = [1,2,3,4,5,6,7,8,9,10,
        11,12,13,14,15,16,17,18,19,20,
        21,22,23,24,25,26,27,28,29,30,31]
years = [2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024]
hours24 = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

# function to get night intervals and day intervals from a single dataframe
# returns two lists of start end tuples, one for nights and one for days
def get_intervals(mouse_df, lights_on, lights_out):
    # get timestamps from mouse df
    dates_range = mouse_df.index            
    #create distinct, consecutive dates
    unique_dates = pd.date_range(mouse_df.index.min().date(),
                                 mouse_df.index.max().date(),
                                 freq='D')
    # mouse day and night intervals      
    night_intervals = []  
    day_intervals = []   
    
    # create night intervals
    # for each date in mouse, create start_hour-end_hour pair 
    # of night interval 
    for j in range(len(unique_dates)):
        # start interval
        start_night = datetime.datetime(unique_dates[j].year, 
                                        unique_dates[j].month, 
                                        unique_dates[j].day,
                                        hour=lights_out,
                                        minute=0,
                                        second=0)
        end_night_before = datetime.datetime(unique_dates[j].year, 
                                             unique_dates[j].month, 
                                             unique_dates[j].day,
                                             hour=lights_on,
                                             minute=0,
                                             second=0)
        # make sure it is not the last inteval
        if (j+1) < len(unique_dates):
            # end interval
            end_night_next = datetime.datetime(unique_dates[j+1].year, 
                                               unique_dates[j+1].month, 
                                               unique_dates[j+1].day,
                                               hour=lights_on,
                                               minute=0,
                                               second=0)
        else: # if it is last interval
            if start_night < dates_range[-1]:
                night_intervals.append((start_night, dates_range[-1]))
            break
        if j == 0: # for the first interval
            if end_night_before > dates_range[0]:
                temp0 = dates_range[0]
                temp1 = end_night_before
                night_intervals.append((temp0,temp1))
                # next night interval strats on the same date
                temp0 = start_night
                temp1 = end_night_next if end_night_next <= dates_range[-1] else dates_range[-1]
                night_intervals.append((temp0,temp1))                        
            else:
                temp0 = start_night
                # if the next date is in the list, 
                # set it to the end of nighttime, 
                # if not set the end of plot to be the end of nighttime
                temp1 = end_night_next if end_night_next <= dates_range[-1] else dates_range[-1]
                night_intervals.append((temp0,temp1))                        
        else:   # not the first day
            temp0 = start_night
            temp1 = end_night_next if end_night_next <= dates_range[-1] else dates_range[-1]
            night_intervals.append((temp0,temp1))
                
    # invert night intervals to get days                    
    for j in range(len(night_intervals)):
        start_night, end_night = night_intervals[j]
        # if it is the first interval
        if j==0:
            # if night starts later than the start of the timestamps
            if start_night > dates_range[0]:
                start_day = dates_range[0]
                end_day = start_night
                day_intervals.append((start_day,end_day))
            else:
                # check if this is not the only interval
                if j+1 < len(night_intervals):
                    start_day = end_night
                    end_day = night_intervals[j+1][0]
                    day_intervals.append((start_day,end_day))
                else: # if it was the only interval
                    if end_night < dates_range[-1]:
                        start_day = end_night
                        end_day = dates_range[-1]
                        day_intervals.append((start_day,end_day))                                
        # check if it was the last interval
        elif j+1 == len(night_intervals): 
            if len(day_intervals) > 1:
                if end_night < dates_range[-1]:
                    start_day = end_night
                    end_day = dates_range[-1]
                    day_intervals.append((start_day,end_day))
        else:
            start_day = end_night
            end_day = night_intervals[j+1][0]
            day_intervals.append((start_day,end_day))

    return night_intervals, day_intervals   
         
        
class FedApp(Toplevel):

    def __init__(self, parent, title = None):

        Toplevel.__init__(self, parent)
        self.transient(parent)

        if title:
            self.title(title)            

        self.parent = parent
        self.retrieved_id_ints = None
        
        # instantiate window with options and make it in focus
        body = Frame(self)
        self.initial_focus = self.body(body)
        body.pack(padx=5, pady=5)

        # instantaite buttons ok and cancel
        self.buttonbox()

        self.grab_set()

        if not self.initial_focus:
            self.initial_focus = self

        self.protocol("WM_DELETE_WINDOW", self.cancel)

        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                  parent.winfo_rooty()+50))

        self.initial_focus.focus_set()

        self.wait_window(self)


    def body(self, master):
        # dialog body.  return widget with options that should have
        # initial focus.  

        Label(master, text="Select folder with csv files:").grid(row=0,
                                                         columnspan=4,
                                                         sticky=W,
                                                         padx=5,
                                                         pady=15)
        Label(master, text="Lights on at:").grid(row=2, column=0, sticky=W, padx=5, pady=5)
        Label(master, text="Lights out at:").grid(row=3, sticky=W, padx=5, pady=5)
        Label(master, text="Select your dates  and hours (mm/dd/yyyy h):").grid(row=4,
                                                         columnspan=3,
                                                         sticky=W, 
                                                         padx=5, 
                                                         pady=20)
        Label(master, text="Month").grid(row=5, column=1, padx=5, pady=5)
        Label(master, text="Day").grid(row=5, column=3, padx=5, pady=5)
        Label(master, text="Year").grid(row=5, column=5, padx=5, pady=5)
        Label(master, text="Hour").grid(row=5, column=6, padx=5, pady=5)
        Label(master, text="From:").grid(row=6, column=0, sticky=W, padx=5, pady=5)
        Label(master, text="/").grid(row=6, column=2, sticky=W, padx=5, pady=5)
        Label(master, text="/").grid(row=6, column=4, sticky=W, padx=5, pady=5)
        Label(master, text="Until:").grid(row=7, column=0, sticky=W, padx=5, pady=5)
        Label(master, text="/").grid(row=7, column=2, sticky=W, padx=5, pady=5)
        Label(master, text="/").grid(row=7, column=4, sticky=W, padx=5, pady=5)
        Label(master, text="*Includes both above dates!", fg="red").grid(row=8, columnspan=3, sticky=W, padx=5, pady=5)
        Label(master, text="Bin size for histograms:\n(in minutes)").grid(row=9, sticky=W, padx=5, pady=5)
        
        self.folder_value = StringVar()
        self.lights_on_default = IntVar(value=6)
        self.lights_out_default = IntVar(value=6)
        self.lights_on_am_pm_default = StringVar(value="am")
        self.lights_out_am_pm_default = StringVar(value="pm")
        self.month_from_default = IntVar(value=3)
        self.day_from_default = IntVar(value=8)
        self.year_from_default = IntVar(value=2018)
        self.hour_from_default = IntVar(value=18)
        self.month_until_default = IntVar(value=3)
        self.day_until_default = IntVar(value=10)
        self.year_until_default = IntVar(value=2018)
        self.hour_until_default = IntVar(value=18)
        self.bin_size_default = IntVar(value=60)
        
        self.select_folder_btn = Button(master, text="Select", command=self.show_folders)
        self.select_folder_btn.grid(row=1,column=5, padx=5, pady=5)
    
        self.folder_path = Entry(master, textvariable=self.folder_value, width=60)
        self.lights_on = OptionMenu(master, self.lights_on_default, *hours_months)
        self.lights_on['bg'] = "#FFF994"
        self.lights_on_am_pm = OptionMenu(master, self.lights_on_am_pm_default, *am_pm)
        self.lights_out = OptionMenu(master, self.lights_out_default, *hours_months)
        self.lights_out['bg'] ="#689CEB"
        self.lights_out_am_pm = OptionMenu(master, self.lights_out_am_pm_default, *am_pm)
        self.month_from = OptionMenu(master, self.month_from_default, *hours_months)
        self.day_from = OptionMenu(master, self.day_from_default, *days)
        self.year_from = OptionMenu(master, self.year_from_default, *years)
        self.hour_from = OptionMenu(master, self.hour_from_default, *hours24)
        self.month_until = OptionMenu(master, self.month_until_default, *hours_months)
        self.day_until = OptionMenu(master, self.day_until_default, *days)
        self.year_until = OptionMenu(master, self.year_until_default, *years)
        self.hour_until = OptionMenu(master, self.hour_until_default, *hours24)
        self.bin_size = Entry(master, textvariable=self.bin_size_default)
    
        self.folder_path.grid(row=1, columnspan=5, sticky=W, padx=5, pady=15)
        self.lights_on.grid(row=2, column=1, padx=5, pady=5)
        self.lights_out.grid(row=3, column=1, padx=5, pady=5)
        self.lights_on_am_pm.grid(row=2, column=2, columnspan=4, sticky=W, padx=5, pady=5)
        self.lights_out_am_pm.grid(row=3, column=2, columnspan=4, sticky=W, padx=5, pady=5)
        self.month_from.grid(row=6,column=1, padx=5, pady=5)
        self.day_from.grid(row=6, column=3, padx=5, pady=5)
        self.year_from.grid(row=6,column=5, padx=5, pady=5)
        self.hour_from.grid(row=6, column=6, padx=5, pady=5)
        self.month_until.grid(row=7,column=1, padx=5, pady=5)
        self.day_until.grid(row=7, column=3, padx=5, pady=5)
        self.year_until.grid(row=7,column=5, padx=5, pady=5)
        self.hour_until.grid(row=7, column=6, padx=5, pady=5)
        self.bin_size.grid(row=9, column=1, columnspan=2, sticky=W, padx=5, pady=5)
        
    
        self.plot_checkbox = IntVar(value=1)
        self.cb = Checkbutton(master, 
                              text="Save Plots", 
                              variable=self.plot_checkbox)
        self.cb.grid(row=11, column=0, columnspan=3, sticky=E, padx=5, pady=5)
        self.data_checkbox = IntVar(value=1)
        self.cb = Checkbutton(master, 
                              text="Save Data", 
                              variable=self.data_checkbox)
        self.cb.grid(row=11, column=3, columnspan=3, sticky=W, padx=5, pady=5)

    def buttonbox(self):
        # add standard button box (ok and cancel)

        box = Frame(self)

        w = Button(box, text="Cancel", width=10, command=self.cancel)
        w.pack(side=LEFT, padx=5, pady=15)
        w = Button(box, text="OK", width=10, command=self.ok, default=ACTIVE)
        w.pack(side=LEFT, padx=5, pady=15)

        # same commands with keyboard (enter==ok, esc==cancel)
        self.bind("<Return>", self.ok)
        self.bind("<Escape>", self.cancel)

        box.pack()
        
#########################################
# This is where all the magic is called #
#########################################
        
    def ok(self, event=None):

        if not self.validate():
            self.initial_focus.focus_set() # put focus back
            return

        self.withdraw()
        self.update_idletasks()
        
        # retrieve user input
        self.get_input()
        # close options window
        self.cancel()
        
        # execute main functionality of the script
        self.main_function()
        print
        print("\nDone")
        try:
            self.parent.destroy()
        except:
            return
        
        
#########################################
        

    def cancel(self, event=None):

        # put focus back to the parent window
        self.parent.focus_set()
        self.destroy()


    def validate(self):
        # validate if path was given 
        # if day and night last 12 hours
        # if bin size is integer no larger than 12 hours
        # if given dates are chronological
        
        # check if day duration of day and night is at least an hour
        if self.lights_on_default.get() == 12:
            if self.lights_on_am_pm_default.get()=="am":
                self.my_lights_on = 0
            elif self.lights_on_am_pm_default.get()=="pm":
                self.my_lights_on  = self.lights_on_default.get()
        elif self.lights_on_am_pm_default.get()=="am":
            self.my_lights_on  = self.lights_on_default.get()
        elif self.lights_on_am_pm_default.get()=="pm":
            self.my_lights_on = self.lights_on_default.get()+12
        if self.lights_out_default.get() == 12:
            if self.lights_out_am_pm_default.get()=="am":
                self.my_lights_out = 0
            elif self.lights_out_am_pm_default.get()=="pm":
                self.my_lights_out  = self.lights_out_default.get()
        elif self.lights_out_am_pm_default.get()=="am":
           self.my_lights_out = self.lights_out_default.get()
        elif self.lights_out_am_pm_default.get()=="pm":
             self.my_lights_out = self.lights_out_default.get()+12
        if abs(self.my_lights_on - self.my_lights_out) != 12:
            messagebox.showwarning(
                "Warning!",
                "Day and Night should last 12 hours each!"
                )
            return 0

        try:
            # check in path was provided
            if len(self.folder_path.get()) > 0:
                # test if bin is integer
                int(self.bin_size.get())
            else:
                messagebox.showwarning(
                "Warning!",
                "Remember to select the path.\nBin has to be an integer.\n\nPlease try again."
                )
                return 0
            # check range of bin size (no bigger than 12 hours)
            if int(self.bin_size.get()) <= 0 or int(self.bin_size.get()) > 720:
                messagebox.showwarning(
                "Warning!",
                "Bin size has to be smaller than 12 hours (720 minutes)!"
                )
                return 0
            # check if from date is earlier than until date
            date_from_date = datetime.datetime(self.year_from_default.get(),
                                        self.month_from_default.get(),
                                        self.day_from_default.get(),
                                        hour=self.hour_from_default.get(),
                                        minute=0,second=0)
            date_until_date = datetime.datetime(self.year_until_default.get(),
                                        self.month_until_default.get(),
                                        self.day_until_default.get(),
                                        hour=self.hour_until_default.get(),
                                        minute=0,second=0)
            if date_from_date < date_until_date:
                return 1
            else:
                messagebox.showwarning(
                "Warning!",
                "From date has to be before Until date!"
                )
                return 0
        except ValueError:
            messagebox.showwarning(
                "Warning!",
                "Remember to select the path.\nBin has to be an integer.\n\nPlease try again."
            )
            return 0

    def get_input(self):
        # executed after clicking on ok button

        self.main_folder_path = self.folder_path.get()
        date_from_str = (str(self.year_from_default.get()) + "-" +
                        str(self.month_from_default.get()) + "-" +
                        str(self.day_from_default.get()) + " " +
                        str(self.hour_from_default.get()) + ":00:00")
        date_until_str = (str(self.year_until_default.get()) + "-" +
                        str(self.month_until_default.get()) + "-" +
                        str(self.day_until_default.get()) + " " +
                        str(self.hour_until_default.get()) + ":00:00")
        self.my_start_date = date_from_str
        self.my_end_date = date_until_str
        self.my_bin_size = self.bin_size.get()
        self.to_plot = False if self.plot_checkbox.get()==0 else True
        self.to_save_data = False if self.data_checkbox.get()==0 else True
        
        
    def show_folders(self):
        # executed when select button in dialog box is clicked
        # select folder from explorer window
        self.src = filedialog.askdirectory()
        self.folder_value.set(self.src)
        
        
    def select_mice(self):
        
        
        # create a list of available mice ids
        self.mice_ids_str_values = ""
        for i in range(len(self.mice_ids_list)):
            if i+1 == len(self.mice_ids_list):   # if the last one
                self.mice_ids_str_values = self.mice_ids_str_values + str(self.mice_ids_list[i])
            else:
                self.mice_ids_str_values = self.mice_ids_str_values + str(self.mice_ids_list[i]) + ","
        
        # create option window
        self.option_window = Tk()
        self.option_window.title('Mice Selection')
        Label(self.option_window, text="Select your mice from the list of available mice:").grid(row=0, column=0, sticky=W, padx=5, pady=5)
        Label(self.option_window, text=self.mice_ids_str_values).grid(row=1, column=0, padx=5, pady=5)
        self.mice_selection = Entry(self.option_window, textvariable="")
        # clear entry just in case, and set the text to mice ids from files
        self.mice_selection.delete(0, END)
        self.mice_selection.insert(0, self.mice_ids_str_values)
        self.mice_selection.grid(row=2, column=0, padx=5, pady=5)
        Label(self.option_window, text="*List of coma separated integer ids! No spaces!", fg="red").grid(row=3, column=0, sticky=W, padx=5, pady=5)
        
        b = Button(self.option_window, text='Ok', command=self.get_mice_choice)
        b.grid(row=4, column=0, sticky='nsew', padx=20, pady=5)
        
        self.option_window.initial_focus = self.option_window
        
        self.option_window.wait_window(self.option_window)

        
    def get_mice_choice(self):
        try:
            # remove leading and trailing whitespaces and comas, and split by comas
            retrieved_id_strings = self.mice_selection.get().strip().strip(',').split(',')
            # check if all options are integers
            self.retrieved_id_ints = [int(el) for el in retrieved_id_strings]
            # check if all options were available in file
            for el in self.retrieved_id_ints:
                if str(el) not in self.mice_ids_str_values.split(','):
                    messagebox.showwarning(
                            "Warning!",
                            "Some of the given ids might not be available in the files."
                    )
                    self.option_window.destroy()
                    # reset ids to none
                    self.retrieved_id_ints = None
                    return
        except:
             messagebox.showwarning(
                "Warning!",
                "List of coma separated integer ids!\nNo spaces!\n"
            )
             self.option_window.destroy()
             # reset ids to none
             self.retrieved_id_ints = None
             return
        self.option_window.destroy()


#########################################################################
# My sequence of actions (reading, binning, plotting)       
    def main_function(self):
        
        csv_read = self.read_csv_files()
        
        # if the csv_read function's checks failed
        if csv_read == 0:
            return 0
        
        
        # read which mice to include 
        self.select_mice()
        # if selected mice were not correctly validated, end here
        if self.retrieved_id_ints is None:
            print("Failed to select mice")
            return
        
        # create a new list of dataframes only with the selected mice
        self.include_selected_mice()

        # retrieve totals for each day, each day/night
        # and interval times between pellet intakes
        self.get_data()
        
        # create path to subfolder for results
        # MonthDay_HourMinute + the above ending + .xls
        current_time = strftime("%m%d_%H%M_%S")+"s"
        subfolder_name = "Results"+current_time
        self.subfolder_path = os.path.join(self.main_folder_path, subfolder_name)
        # if folder not yet created, create one
        if (not os.path.exists(self.subfolder_path)):
            os.mkdir(self.subfolder_path)
            
        # plot binned pellets and motorturns and intervlas (one plot per mouse)
        self.plot_pellets_and_motorturns() 
        
        if (self.to_plot):
            # plot histograms
            self.plot_histograms()
            self.plot_kcal()
            
            
        if (self.to_save_data):
            # save day data and day/night data
            self.save_data()
        
#############################################################################   
#####################################################
# FUNCTION TO READ AND SORT EACH MOUSE DATA, 
# AND TO GET ALL MICE IDS
    def read_csv_files(self):
        # reads csv files and organizes them into dataframes
        
        all_dataframes = []
        # for all files in folder
        for file in os.listdir(self.main_folder_path):
            if file.endswith(".CSV"):
                if file.startswith("FED"):
                    # read that file into a dataframe
                    file_path = os.path.join(self.main_folder_path ,file)
                    df = pd.read_csv(file_path)
                    all_dataframes.append(df)
##################################################                    
        # create a single dataframe from all files
        self.main_df = pd.concat(all_dataframes)
        print(self.main_df)
##################################################
        
        # create separate dataframe for each mouse
        # (all original columns)
        by_mouse_df_list = []
        
        # find unique mouse indexes
        mice_indexes = pd.unique(self.main_df[' Mouse'])
        # split main dataframe into single dataframe per mouse
        for index in mice_indexes:
            single_mouse_df = self.main_df[self.main_df[' Mouse']==index]
            by_mouse_df_list.append(single_mouse_df)
            
########################################################################        
        # list of dataframes by mouse (only given dates)
        # (only sorted timestamps, mouse index, pellet count, motorturn count)
        self.mouse_df_list = []
########################################################################
            
        # make sure all dates are sorted:
        for i in range(len(by_mouse_df_list)):
            # count how many rows are there
            # that is equal to the total pellet count
            total_pellet_count = by_mouse_df_list[i].shape[0]
            # create consecutive pellet count values
            total_pellet_count_list = [i+1 for i in range(total_pellet_count)]
            # convert dates to pandas datetime
            ts_list = pd.to_datetime(by_mouse_df_list[i]['MM:DD:YYYY hh:mm:ss']).tolist()
            # create new dataframe
            new_df = pd.DataFrame({"MM:DD:YYYY hh:mm:ss" :ts_list,
                                   "Mouse" : by_mouse_df_list[i][' Mouse'].tolist(),
                                   "PelletCount" : total_pellet_count_list,
                                   "MotorTurns" : by_mouse_df_list[i][' MotorTurns'].tolist()})
            # make timestamps indexes
            new_df.index = new_df['MM:DD:YYYY hh:mm:ss']
            # remove old column
            del new_df['MM:DD:YYYY hh:mm:ss']
            # sort dates
            new_df = new_df.sort_index()
            
            # select only user defined timeframe
            # https://pandas.pydata.org/pandas-docs/stable/timeseries.html
            new_df = new_df[self.my_start_date:self.my_end_date]
            # replace pellet count with new consecutive pellet count for that dates
            new_df['PelletCount'] = [i+1 for i in range(new_df.shape[0])]
            if new_df.shape[0] != 0:
                self.mouse_df_list.append(new_df)
            else:
                # if for a mouse, there is no data within given dates
#                my_start_year,my_start_month,my_start_day = self.my_start_date.split('-')
#                my_end_year,my_end_month,my_end_day = self.my_end_date.split('-')
                # create dataframe with all zero values
                start = datetime.datetime.strptime(self.my_start_date, "%Y-%m-%d %H:%M:%S")
                end = datetime.datetime.strptime(self.my_end_date, "%Y-%m-%d %H:%M:%S")
                new_df = pd.DataFrame({"MM:DD:YYYY hh:mm:ss" :[start,end],
                                       "Mouse" : [by_mouse_df_list[i][' Mouse'].iloc[0], by_mouse_df_list[i][' Mouse'].iloc[0]],
                                       "PelletCount" : [0,0],
                                       "MotorTurns" : [0,0]})
                new_df.index = new_df['MM:DD:YYYY hh:mm:ss']
                del new_df['MM:DD:YYYY hh:mm:ss']
                new_df = new_df.sort_index()
                self.mouse_df_list.append(new_df)

        # check if there was any data    
        if len(self.mouse_df_list) == 0:
            messagebox.showwarning(
                "Warning!",
                "No data for given dates!"
                )
            return 0

            
        # get all mice ids from dataframes
        self.mice_ids_list = []
        for i in range(len(self.mouse_df_list)):
            mouse_id = self.mouse_df_list[i]['Mouse'].iloc[0]
            if mouse_id not in self.mice_ids_list:
                self.mice_ids_list.append(mouse_id)
                
                     
        return 1   
    
    def include_selected_mice(self):
        included_mice_df = []
        for i in range(len(self.mouse_df_list)):
            # get mouse id from the dataframe
            mouse_id = self.mouse_df_list[i]['Mouse'].iloc[0]
            # check if that is was selected by user
            if mouse_id in self.retrieved_id_ints:
                included_mice_df.append(self.mouse_df_list[i])
        # make new list of dataframes only with selected mice a main source of data
        self.mouse_df_list = included_mice_df

##################################################################
# FUNCTION TO GET DAY AND NIGHT INTERVALS FOR EACH MOUSE
# TO CALCULATE TOTAL PELLET INTAKE BY 24HRS AND AVERAGE,
# TOTAL PELLET INTAKE DURING DAYTIMES AND NIGHTTIMES AND AVERAGE
# INTERVALS BETWEEN PELLET INTAKES
    def get_data(self):
        
        ################################################################ 
        # day and night intervals by mouse
        self.night_mouse_intervals = []
        self.day_mouse_intervals = []

######################################################################
        # for each mouse get all night intervals and all day intervals
        for i in range(len(self.mouse_df_list)):
            
            # single mouse 
            night_intervals, day_intervals = get_intervals(self.mouse_df_list[i],
                                                           self.my_lights_on, 
                                                           self.my_lights_out)                                    
            # add to the list of intervals for all mice
            self.night_mouse_intervals.append(night_intervals)
            self.day_mouse_intervals.append(day_intervals)
            ######## end creating all day intervals for that mouse
            ######## (self.day_mouse_intervals)
        
        # find first date of all and last date from all
        starts = []
        ends = []
        # for all mice find beginning and end of data
        for df in self.mouse_df_list:            
            # find first date for that mouse and the last date
            starts.append(df.index.min())
            ends.append(df.index.max())
            
        # find the earliest date from all mice and the lates date from all mice    
        earliest_of_all = min(starts)
        latest_of_all = max(ends)
#        print(earliest_of_all, latest_of_all)
        
        # create the list of start times for all available 24 hour periods
        
         # first, find whether the earliest common date is closer to start day or start night
        only_date_earliest = earliest_of_all.date()
        that_day = pd.Timestamp(year=only_date_earliest.year, 
                                month=only_date_earliest.month, 
                                day=only_date_earliest.day, 
                                hour=int(self.my_lights_on))

        that_night = pd.Timestamp(year=only_date_earliest.year, 
                                month=only_date_earliest.month, 
                                day=only_date_earliest.day, 
                                hour=int(self.my_lights_out))
        # decide whether to start from the lights out or lights on hour
        if abs(that_day-earliest_of_all) < abs(that_night-earliest_of_all):
            my_earliest = that_day
        else:
            my_earliest = that_night
            
        all_24hrs_dates = pd.date_range(my_earliest,
                                       latest_of_all,
                                       freq='D')

        # create a dataframe for each mouse 
        # that contains data from common start and common end
        mouse_per24hrs_full_dfs = []
        all_mouse_day_intervals = []
        all_mouse_nigtht_intervals = []
        for df in self.mouse_df_list:
            new_df = df[earliest_of_all:latest_of_all]
            mouse_per24hrs_full_dfs.append(new_df)
            # get all night and day intervals for that mouse
            night_intervals, day_intervals = get_intervals(new_df,
                                                           self.my_lights_on, 
                                                           self.my_lights_out)
            all_mouse_day_intervals.append(day_intervals)
            all_mouse_nigtht_intervals.append(night_intervals)

        # for each mouse create list of dataframes that contain timestamps 
        # for each 24 hour period
        mouse_24hrs_dfs = []
        for i in range(len(mouse_per24hrs_full_dfs)):
            # list of tuples,
            # first element is a pair of start-end period,
            # second element is dataframe, one df per each 24 hour period
            mouse_dfs = []
            for j in range(len(all_24hrs_dates)):
                # if it is the first beginning
                if j == 0:
                    # check if this is the only beginning
                    if len(all_24hrs_dates) == 1:
                        start = all_24hrs_dates[j]
                        # ends on last available time
                        end = mouse_per24hrs_full_dfs[i].index.max()
                        single_period_df = mouse_per24hrs_full_dfs[i][start:end]
                        if single_period_df.empty:
                            # if there was no data for that interval create a dummy
                            single_period_df = pd.DataFrame({"MM:DD:YYYY hh:mm:ss" :[start,end],
                                                               "Mouse" : [mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0], mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0]],
                                                               "PelletCount" : [0,0],
                                                               "MotorTurns" : [0,0]})
                            single_period_df.index = single_period_df['MM:DD:YYYY hh:mm:ss']
                            del single_period_df['MM:DD:YYYY hh:mm:ss']
                        mouse_dfs.append(((start,end),single_period_df))
                    else:   # this was not the only beginning (not the last)
                        start = all_24hrs_dates[j]
                        end = all_24hrs_dates[j+1]
                        single_period_df = mouse_per24hrs_full_dfs[i][start:end]
                        if single_period_df.empty:
                            # if there was no data for that interval create a dummy
                            single_period_df = pd.DataFrame({"MM:DD:YYYY hh:mm:ss" :[start,end],
                                                               "Mouse" : [mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0], mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0]],
                                                               "PelletCount" : [0,0],
                                                               "MotorTurns" : [0,0]})
                            single_period_df.index = single_period_df['MM:DD:YYYY hh:mm:ss']
                            del single_period_df['MM:DD:YYYY hh:mm:ss']
                        mouse_dfs.append(((start,end),single_period_df))
                # check if it was the last beginning
                elif (j+1) == len(all_24hrs_dates):
                    
                    start = all_24hrs_dates[j]
                    end = mouse_per24hrs_full_dfs[i].index.max()
                    # check if the start date is earlier that the end of data
                    if start < end:
                        
                        single_period_df = mouse_per24hrs_full_dfs[i][start:end]
                        if single_period_df.empty:
                            # if there was no data for that interval create a dummy
                            single_period_df = pd.DataFrame({"MM:DD:YYYY hh:mm:ss" :[start,end],
                                                                   "Mouse" : [mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0], mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0]],
                                                                   "PelletCount" : [0,0],
                                                                   "MotorTurns" : [0,0]})
                            single_period_df.index = single_period_df['MM:DD:YYYY hh:mm:ss']
                            del single_period_df['MM:DD:YYYY hh:mm:ss']
                        mouse_dfs.append(((start,end),single_period_df))
                else:   # not the first and not the last beginning
                    start = all_24hrs_dates[j]
                    end = all_24hrs_dates[j+1]
                    single_period_df = mouse_per24hrs_full_dfs[i][start:end]
                    if single_period_df.empty:
                        # if there was no data for that interval create a dummy
                        single_period_df = pd.DataFrame({"MM:DD:YYYY hh:mm:ss" :[start,end],
                                                               "Mouse" : [mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0], mouse_per24hrs_full_dfs[i]['Mouse'].iloc[0]],
                                                               "PelletCount" : [0,0],
                                                               "MotorTurns" : [0,0]})
                        single_period_df.index = single_period_df['MM:DD:YYYY hh:mm:ss']
                        del single_period_df['MM:DD:YYYY hh:mm:ss']
                    mouse_dfs.append(((start,end),single_period_df))
            mouse_24hrs_dfs.append(mouse_dfs)
            
#            print(mouse_dfs)
                   
        # create dataframes for the csv files
        # for all mice, create dataframe with start dates as indexes
        # column names as mice ids with sums from that 24 hours in each row
        # last column with means
        # divide each day into sums from daytime and nighttime
        mice_by_24hrs_cumm_dfs = []
        mice_by24h_day_night_dfs = []
        for i in range(len(mouse_24hrs_dfs)): # for each mouse
            my_sums = []
            my_day_sums = []
            my_night_sums = []
#            print("\nMouse", i+1)
            for j in range(len(mouse_24hrs_dfs[i])): # for each day           
                # get a list of either day or night according to hours from timestamp
                day_night_list = []
                for row in mouse_24hrs_dfs[i][j][1].itertuples():
                    if row[0].hour >= self.my_lights_on and row[0].hour < self.my_lights_out:
                        day_night_list.append('day')
                    else:
                        day_night_list.append('night')
                # create new dataframe with a column for days and nights
                # first element of tuple is pair of dates
                # second element is dataframe
                day_night_df = mouse_24hrs_dfs[i][j][1].copy()
                day_night_df['DayNight'] = day_night_list
                # (if sum=0, there were no data)
                if day_night_df[day_night_df.DayNight == 'day']['PelletCount'].sum() == 0:
                    my_day_sum = np.nan
                else:
                    my_day_sum = day_night_df[day_night_df.DayNight == 'day'].shape[0]
                if day_night_df[day_night_df.DayNight == 'night']['PelletCount'].sum() == 0:
                    my_night_sum = np.nan
                else:
                    my_night_sum = day_night_df[day_night_df.DayNight == 'night'].shape[0]
                # second element is dataframe
                # first sum pellets (if sum=0, there were no data)
                if mouse_24hrs_dfs[i][j][1]['PelletCount'].sum() == 0:
                    my_sum = np.nan
                else:   # number of rows of data is the sum of all pellets
                    my_sum = mouse_24hrs_dfs[i][j][1].shape[0]
                mouse_name = "Mouse " + str(mouse_24hrs_dfs[i][j][1]['Mouse'].iloc[0])
                mouse_name_day = mouse_name + "_Day"
                mouse_name_night = mouse_name + "_Night"
                my_sums.append(my_sum)
                my_day_sums.append(my_day_sum)
                my_night_sums.append(my_night_sum)
            df = pd.DataFrame({mouse_name:my_sums})
            day_night_df = pd.DataFrame({mouse_name_day:my_day_sums,
                                         mouse_name_night:my_night_sums})
            mice_by_24hrs_cumm_dfs.append(df)
            mice_by24h_day_night_dfs.append(day_night_df)
            
        # join all single mice data into one, common dataframe
        self.mice_by_24hrs_df = pd.concat(mice_by_24hrs_cumm_dfs, axis=1)
        # join all day/night dataframes into a single dataframe
        self.mice_by_daynight_df = pd.concat(mice_by24h_day_night_dfs, axis=1)
                
        # replace all nans with zero values
        self.mice_by_24hrs_df.replace(np.nan,value=0, inplace=True)
        self.mice_by_daynight_df.replace(np.nan,value=0, inplace=True)
    
        # calculate means
        self.mice_by_24hrs_df['Means'] = self.mice_by_24hrs_df.mean(axis=1)
        # calculate means for daytimes
        self.mice_by_daynight_df['Means_Day'] = self.mice_by_daynight_df.filter(like='_Day').mean(axis=1)
        self.mice_by_daynight_df['Means_Night'] = self.mice_by_daynight_df.filter(like='_Night').mean(axis=1)
        
        # add beginnings of 24 hour intervals 
        # (last date on the list could be the end of all data)
        my_indexes = all_24hrs_dates[:-1] if len(all_24hrs_dates[:-1])==self.mice_by_24hrs_df.shape[0] else all_24hrs_dates
        self.mice_by_24hrs_df['24h Day Start'] = my_indexes
        # make the dates indexes
        self.mice_by_24hrs_df.index = self.mice_by_24hrs_df['24h Day Start']
        del self.mice_by_24hrs_df['24h Day Start']
#        print (mice_by_24hrs_df)
        # do the same for day/night df
        self.mice_by_daynight_df['24h Day Start'] = my_indexes
        self.mice_by_daynight_df.index = self.mice_by_daynight_df['24h Day Start']
        del self.mice_by_daynight_df['24h Day Start']
        #############################################
        ## end creating self.mice_by_24hrs_df with group averages
        ## end creating self.mice_by_daynight_df with group averages
        
###############################################################
# get intervals between pellets        
        # create a list of intervals between pellet intakes (in minutes)
        # one list per mouse
        self.intervals_in_minutes = [[] for i in range(len(self.mouse_df_list))]
        for i in range(len(self.mouse_df_list)):
            for j in range(len(self.mouse_df_list[i].index)):
                # if it is not the last one
                if j+1 < len(self.mouse_df_list[i].index):
                    interval = self.mouse_df_list[i].index[j+1]-self.mouse_df_list[i].index[j]                
                    # if by any chance there were more than one pellets in a single second
                    # assert it was 1 second
                    if interval.total_seconds() == 0:
                        self.intervals_in_minutes[i].append(float(1.0)/60.0)
                    else:
                        # convert to total number of seconds and divide by 60 to get minutes
                        self.intervals_in_minutes[i].append(float(interval.total_seconds())/60.0)
         
        # find common min and max interval
        # in order to use the same scale for all interval histograms             
        all_max = []
        all_min = []
        for i in range(len(self.intervals_in_minutes)):
            all_max.append(max(self.intervals_in_minutes[i]))
            all_min.append(min(self.intervals_in_minutes[i]))        
        self.max_interval = max(all_max)
        self.min_interval = min(all_min)

##############################################
# FUNCTION TO PLOT PELLETS AND MOTORTURNS        
    def plot_pellets_and_motorturns(self):
        if (self.to_plot):
            print("\nPlotting Pellets...\n")
        # collect values for binned intervals for each mouse
        intervals_from_hist_by_mouse = []
        # create a common log10 xaxis scale for interval histogram
        # https://stackoverflow.com/questions/47850202/plotting-a-histogram-on-a-log-scale-with-matplotlib?rq=1
        # Use non-equal bin sizes, such that they look equal on log scale. 
        logbins = np.logspace(np.log10(self.min_interval),
                              np.log10(self.max_interval),
                              num=50)
        for i in range(len(self.mouse_df_list)):
            # plot only if there are any data
            if self.mouse_df_list[i]['MotorTurns'].sum() != 0 and self.mouse_df_list[i]['PelletCount'].sum() != 0:
                mouse_name = "Mouse " + str(self.mouse_df_list[i]['Mouse'].iloc[0])
                unique_dates = pd.date_range(self.mouse_df_list[i].index.min().date(),
                                         self.mouse_df_list[i].index.max().date(),
                                         freq='D')
                total_days = len(unique_dates)
                if (self.to_plot):
                    print(mouse_name, ":", total_days, "days")
                    # get first and last timestamp for each mouse
                    print("Start:", self.mouse_df_list[i].index.min())
                    print("End:", self.mouse_df_list[i].index.max())
                    
                # convert pandas dates to something matplotlib understands
                dates2plot = self.mouse_df_list[i].index
                # relpace zero values with nans, in order not to plot them
                motorturns = self.mouse_df_list[i]['MotorTurns'].replace(0,value=np.nan)
                pellets = self.mouse_df_list[i]['PelletCount'].replace(0,value=np.nan)

                fig = plt.figure(figsize=(12,6)) 
                ax = plt.subplot2grid((3,3),(0,0), rowspan=2, colspan=2)
                my_title = mouse_name + " (" + str(total_days) + " days)"
#                plt.title(my_title, fontsize=16)
                fig.suptitle(my_title, fontsize=16)
                plt.ylabel("Pellets", fontsize=12)
                # plot pelletcount
                ax.scatter(dates2plot, pellets,
                                   color='b',
                                   s=100, marker='|', label = 'Cumm. Pellets')
        
                # shade night intervals
                for interval in self.night_mouse_intervals[i]:
                    t0, t1 = interval 
                    ax.axvspan(t0, t1, alpha=0.2, facecolor='gray')
                
                plt.yticks(fontsize=8)
                plt.ylim(0,pellets.max())
                plt.xlim(min(dates2plot),max(dates2plot))
                ax.axes.get_xaxis().set_visible(False)
                ax.legend(loc='lower right', fontsize=8)
                
                # add second subplot with motorturns
                ax2 = plt.subplot2grid((3,3),(2,0), rowspan=1, colspan=2, sharex=ax)
                # plot motorturns
                ax2.scatter(dates2plot, motorturns,
                                   color='g',
                                   s=50, marker='o', 
                                   label = 'Motorturns')
                # shade night intervals
                for interval in self.night_mouse_intervals[i]:
                    t0, t1 = interval
                    ax2.axvspan(t0, t1, alpha=0.2, facecolor='gray')
                plt.ylabel('Motorturns', fontsize=12)
#                ax2.yaxis.set_label_position("right")
                plt.xlabel("Daytimes and Nighttimes", fontsize = 12)
                ax2 = plt.gca()  # get the current axes
                # format of date displayed on the x axis
                # what ticks will be visible
                xfmt = md.DateFormatter('%m/%d\n%H:%M')
                ax2.xaxis.set_major_formatter(xfmt)
                major_hour = md.HourLocator(byhour=self.my_lights_out, interval=1)
                ax2.xaxis.set_major_locator(major_hour)
                plt.xticks(rotation=50,fontsize=8)
                plt.yticks(fontsize=8)
                plt.xlim(min(dates2plot),max(dates2plot))
                # force only integer ticks
                ax2.yaxis.set_major_locator(MaxNLocator(integer=True, nbins=5))
                ax2.legend(loc='upper right', fontsize=8)
                
                # add third subplot
                ax3 = plt.subplot2grid((3,3),(0,2), rowspan=3, colspan=1)
#                ax3.hist(self.intervals_in_minutes[i], histtype='step')
                values, bins, patches = ax3.hist(self.intervals_in_minutes[i], 
                         bins=logbins, 
                         histtype='step', color='k')
                plt.xscale('log')
                plt.ylabel("Inter-pellet Intervals", fontsize=12)
                plt.xlabel("Minutes (log10)", fontsize = 12)
                plt.yticks(fontsize=8)
                # force only integer ticks
                ax3.yaxis.set_major_locator(MaxNLocator(integer=True))
                plt.xticks(fontsize=8)
                # align subplots
                plt.subplots_adjust(left=0.08, bottom=0.15, right=0.97, top=0.90, wspace=0.3, hspace=0.3)
               
                # create dataframe with binned values in order to save in csv
                df = pd.DataFrame({'bin (min)':bins[:-1], mouse_name:values})
                df.index = df['bin (min)']
                del df['bin (min)']
                intervals_from_hist_by_mouse.append(df)
                
                if (self.to_plot):
                    # save plot
                    filename = mouse_name + ".png"
                    path_to_save = os.path.join(self.subfolder_path,filename)
                    plt.savefig(path_to_save, format='png', dpi=1000)
                    plt.close(fig)
            
        # join single mice dataframes with intervals      
        intervals_from_hist2save = pd.concat(intervals_from_hist_by_mouse,
                                                 axis=1)
        # add column with mean
        intervals_from_hist2save['Mean'] = intervals_from_hist2save.mean(axis=1)
        # add standard error
        # equivalent to STDEV.S/SQRT(NUMBER OF SAMPLES=mice) in excel
        intervals_from_hist2save['Std_err'] = intervals_from_hist2save.iloc[:,:-1].std(axis=1)/math.sqrt(len(self.mouse_df_list))
        
        if (self.to_save_data):
            # save csv
            intervals_file_name = "GroupIntervals.csv"
            intervals_from_hist2save.to_csv(os.path.join(self.subfolder_path ,intervals_file_name))
            print("\nSaved", os.path.join(self.subfolder_path ,intervals_file_name))
            
        # if user wanted to plot, plot average group inter-pellet interval
        if (self.to_plot):
            print("\nPlotting group inter-pellet intervals...\n")
            fig, ax = plt.subplots()
            group_plot_title = 'Group Average'
            plt.title(group_plot_title, fontsize=16)
            plt.ylabel('Inter-pellet Intervals', fontsize = 12)
            plt.xlabel('Minutes (log10)', fontsize = 12)
            # plot means
            ax.plot(logbins[:-1], intervals_from_hist2save['Mean'], 
                          color='k')
            # get data to plot standard error around average plot
            std_err = intervals_from_hist2save['Std_err'].tolist()
            my_means = intervals_from_hist2save['Mean'].tolist()
            positive_std_err_plot = [my_means[i]+std_err[i] for i in range(len(std_err))]
            negative_std_err_plot = [my_means[i]-std_err[i] for i in range(len(std_err))]
            # plot starndard error
            ax.fill_between(logbins[:-1], positive_std_err_plot, negative_std_err_plot, 
                    alpha=0.5, facecolor='skyblue', edgecolor='b', 
                    linestyle='dotted', linewidth=0.7, label = 'Standard error')
            
            plt.xscale('log')
            plt.yticks(fontsize=8)
            # force only integer ticks
            ax.yaxis.set_major_locator(MaxNLocator(integer=True))
            plt.xticks(fontsize=8)
            ax.legend(loc='upper right', fontsize=8)
            
            # save plot
            filename = "GroupAverage_intervals.png"
            path_to_save = os.path.join(self.subfolder_path,filename)
            plt.savefig(path_to_save, format='png', dpi=1000)
            plt.close(fig)
        

########################################
# FUNCTION TO PLOT HISTOGRAMS 
# FOR INDIVIDUAL MICE AND GROUP AVERAGE
    def plot_histograms(self):
        print("\nPlotting Histograms...\n")   
        # list of dataframes, one df per mouse, 
        # each dataframe contains mouse name column with histogram values
        mice4avg_vals_from_hist = []
        # list of all starts and all ends of dates from all mice
        starts = []
        ends = []
        # for each mouse
        for i in range(len(self.mouse_df_list)):
            mouse_name = "Mouse " + str(self.mouse_df_list[i]['Mouse'].iloc[0])
            # calculate how many minutes are in all data
            total_minutes = math.floor(abs(self.mouse_df_list[i].index.max()-self.mouse_df_list[i].index.min()).total_seconds()/60)
            #create distinct, consecutive dates for mouse data
            unique_dates = pd.date_range(self.mouse_df_list[i].index.min().date(),
                                         self.mouse_df_list[i].index.max().date(),
                                         freq='D')
            total_days = len(unique_dates)
            
            print(mouse_name, ":", total_days, "days")
            # get first and last timestamp for each mouse
            print("Start:", self.mouse_df_list[i].index.min())
            print("End:", self.mouse_df_list[i].index.max())
            
            # plot histograms for single mouse
            fig, ax = plt.subplots(figsize=(10,3))
            my_title = mouse_name + " (" + str(total_days) + " days)"
            plt.title(my_title, fontsize=16)
            plt.ylabel('Pellets', fontsize = 12)
            plt.xlabel("Daytimes and Nighttimes", fontsize = 12)
                
            # calculate how many bins of a specified bin size are for all duration of data
            no_bins = math.floor(total_minutes/int(self.my_bin_size))
            
            # convert pandas dates to something matplotlib understands
            dates2plot = self.mouse_df_list[i].index
                
            values, bins, patches = ax.hist(dates2plot, alpha=0.35, bins= no_bins, 
                                            color="r", label = "Pellet Hist")
         
            if (self.to_save_data):
                # save csv with data from plotted histogram
                my_timestamps = []
                for el in bins[:-1]:
                    my_timestamps.append(md.num2date(el))
                hist_data_df = pd.DataFrame({"bins":my_timestamps,
                                             "values":values})
                hist_mousedata = "Mouse"+ str(i+1)+"_hist_data.csv"
                hist_data_df.to_csv(os.path.join(self.subfolder_path,hist_mousedata))
                print("\nSaved", os.path.join(self.subfolder_path,hist_mousedata))
                print()
            # shade night intervals
            for interval in self.night_mouse_intervals[i]:
                t0, t1 = interval
                ax.axvspan(t0, t1, alpha=0.2, facecolor='gray')
                    
            ax = plt.gca()  # get the current axes
            xfmt = md.DateFormatter('%m/%d\n%H:%M')
#            xfmt = md.DateFormatter('%m/%d')
            ax.xaxis.set_major_formatter(xfmt)
            major_hour = md.HourLocator(byhour=self.my_lights_out, interval=1)
            ax.xaxis.set_major_locator(major_hour)
                
            plt.xticks(rotation=50,fontsize=8)
            plt.yticks(fontsize=8)
            # force only integer ticks
            ax.yaxis.set_major_locator(MaxNLocator(integer=True))
            plt.xlim(min(dates2plot),max(dates2plot))
            ax.legend(loc='upper right', fontsize=8)
            plt.tight_layout()
            
            # save plot
            filename = mouse_name + "_hist.png"
            path_to_save = os.path.join(self.subfolder_path,filename)
            plt.savefig(path_to_save, format='png', dpi=1000)
            plt.close(fig)
            
###########################
# PLOT GROUP HISTOGRAM
            
            mice4avg_vals_from_hist.append(pd.DataFrame({mouse_name:values}))
            # retrieve first and last date 
            # in order to later find first of all and last of all
            starts.append(bins[0])
            # bins is one more than the length of values
            ends.append(bins[-2])
        # find first and last of all        
        # join each mouse values from single histograms into one df
        mice4avg_df = pd.concat(mice4avg_vals_from_hist,axis=1)
        # replace nans with zero values
        mice4avg_df.replace(np.nan, value=0, inplace=True)
        
        # get standard errors
        # equivalent to STDEV.S/SQRT(NUMBER OF SAMPLES=mice) in excel
        mice4avg_df['Std_err'] = mice4avg_df.std(axis=1)/math.sqrt(len(self.mouse_df_list))

        # add a column with means across all mice
        mice4avg_df['Means'] = mice4avg_df.iloc[:,:-1].mean(axis=1)
        
        
        # create dates with as many periods as bins in group histogram
        # calculate the length of each interval between dates (in seconds) to make as many
        # periods as all needed bins
        interval_in_seconds = ((md.num2date(max(ends))-md.num2date(min(starts)))/mice4avg_df.shape[0]).total_seconds()
        frequency = str(int(interval_in_seconds))+"S"

        dates4avg = pd.date_range(md.num2date(min(starts)),
                                       md.num2date(max(ends)),
                                       freq=frequency)
        # add the timestamps as indexes to the group dataframe (exclude last date from dates4avg)
        mice4avg_df['dates'] = dates4avg[:-1]
        # make timestamps indexes
        mice4avg_df.index = mice4avg_df['dates']
        # remove old column
        del mice4avg_df['dates']
        # make sure dates are sorted
        mice4avg_df = mice4avg_df.sort_index()
        # get rid of the timezone data 
        # in order to compare times in get_intervals function
        mice4avg_df.index = mice4avg_df.index.tz_localize(None)
        
        # get night intervals to shade for group average
        group_night_intervals, group_day_intervals = get_intervals(mice4avg_df, 
                                                                   self.my_lights_on, 
                                                                   self.my_lights_out)
        std_err = mice4avg_df['Std_err'].tolist()
        my_means = mice4avg_df['Means'].tolist()
        
        # get data to plot standard error around average plot
        positive_std_err_plot = [my_means[i]+std_err[i] for i in range(len(std_err))]
        negative_std_err_plot = [my_means[i]-std_err[i] for i in range(len(std_err))]
    
        # plot group histogram with dates as xaxis
        fig, ax = plt.subplots(figsize=(10,3))
        group_plot_title = 'Group Average (' + str(total_days) + " days)"
        plt.title(group_plot_title, fontsize=16)
        plt.ylabel('Pellets', fontsize = 12)
        plt.xlabel("Daytimes and Nighttimes", fontsize = 12)
        plt.ylim(0,mice4avg_df['Means'].max())
        plt.xlim(mice4avg_df.index.min(),mice4avg_df.index.max())

        ax.plot(mice4avg_df.index, my_means, color='k', linewidth=1.5)
        ax.fill_between(mice4avg_df.index, positive_std_err_plot, negative_std_err_plot, 
                    alpha=0.5, facecolor='skyblue', edgecolor='b', 
                    linestyle='dotted', linewidth=0.7, label = 'Standard error')

        
        # shade night intervals
        for interval in group_night_intervals:
            t0, t1 = interval            
            ax.axvspan(t0, t1, alpha=0.2, facecolor='gray')
        
        xfmt = md.DateFormatter('%m/%d\n%H:%M')
#        xfmt = md.DateFormatter('%m/%d')
        ax.xaxis.set_major_formatter(xfmt)
        major_hour = md.HourLocator(byhour=self.my_lights_out, interval=1)
        ax.xaxis.set_major_locator(major_hour)
                
        plt.xticks(rotation=50,fontsize=8)
        plt.yticks(fontsize=8)
        # force only integer ticks
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
        # add a legend
        an1 = ax.legend(loc='upper right', fontsize=8)
        # add bin size as text to the legend box
        bin_text = "bin size = " + self.my_bin_size + " min"
        txt = offsetbox.TextArea(bin_text, textprops=dict(size=8))
        box = an1._legend_box
        box.get_children().append(txt)
        box.set_figure(box.figure) 
        plt.tight_layout()
        
        print("\nPlotting group histogram...\n")
        # save plot
        filename = "GroupAverage.png"
        path_to_save = os.path.join(self.subfolder_path,filename)
        plt.savefig(path_to_save, format='png', dpi=1000)
        plt.close(fig)
        
        if (self.to_save_data):
            # save csv with data from plotted histogram
            hist_data = "GroupAverage_hist_data.csv"
            mice4avg_df.to_csv(os.path.join(self.subfolder_path,hist_data), 
                                    na_rep='NA')
            print("\nSaved", os.path.join(self.subfolder_path,hist_data))
        
#######################################################################
# FUNCTION TO PLOT GROUP AVERAGES AS A BAR PLOT WITH KCAL
######################################################        
    def plot_kcal(self):
        
        print("\nPlotting group kcal...\n") 
        # calculate kcal from total 24 hour pellet intake
        kcalories_df = pd.DataFrame()
        kcalories_df['kcal'] = (self.mice_by_24hrs_df['Means']*20/1000)*3.3
        
        # plot a bar plot
        fig, ax = plt.subplots(figsize=(10,3))
        group_plot_title = 'Group Average (' + str(kcalories_df.shape[0]) + " days)"
        plt.title(group_plot_title, fontsize=16)
        plt.ylabel('Pellets (kcal)', fontsize = 12)
        plt.xlabel('Day', fontsize = 12)
        
        days2plot = [i+1 for i in range(kcalories_df.shape[0])]
        y_pos = np.arange(len(days2plot))
        ax.bar(y_pos, kcalories_df['kcal'], align='center', color='g', alpha=0.85)
        plt.xticks(y_pos, days2plot)
        plt.tight_layout()
        
        # save plot
        filename = "GroupAverage_kcal.png"
        path_to_save = os.path.join(self.subfolder_path,filename)
        plt.savefig(path_to_save, format='png', dpi=1000)
        plt.close(fig)
                

###################################################################
# FUNCTION TO SAVE 24 HOUR CUMMULATIVE PELLET COUNTS
# AND DAY/NIGHT PELLET COUNT  
        
    def save_data(self):
        
        # save csv with group day (24hrs) averages
        group_days = "Group_Days.csv"
        self.mice_by_24hrs_df.to_csv(os.path.join(self.subfolder_path ,group_days), 
                                na_rep='NA')
        print("\nSaved", os.path.join(self.subfolder_path ,group_days))
        
        # save csv with day/night averages
        daynight_days = "Group_Day_Night.csv"
        self.mice_by_daynight_df.to_csv(os.path.join(self.subfolder_path ,daynight_days), 
                                na_rep='NA')
        print("\nSaved", os.path.join(self.subfolder_path ,daynight_days))
        
#############################################################################
# CREATE GUI APPLICATION
        
root = Tk()
app = FedApp(root, title="Application Options")

root.mainloop()



    


