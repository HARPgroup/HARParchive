import h5py #import packages
import datetime
f = h5py.File('OR1_7700_7980.h5', 'r') #open h5 file
tsi = input("Enter time series to convert i.e. TS011: ") #ask user which timeseries dataset they wish to convert
dset = f['TIMESERIES/' + tsi + '/table'] #create Dataset object for the timeseries of intrest using previous input
dset_length = dset.shape[0] #pull length of data set from the shape tuple, where number of entries is the first value (position 0 in the tuple)
uts_ts = dset[0:dset_length] #create an array/list of tuples to iterate through

for i, tuple in enumerate(uts_ts):  #for loop iterating through the array, with an indexing variable "i"

    ts_tuple = uts_ts[i] #pull tuple "i" from the array
    uts = (ts_tuple[0])/(1*pow(10, 9)) #divide by 1*10^9 per Rob's suggestion
    dtts = datetime.datetime.fromtimestamp(uts) #use datetime package to convert from unix time to date time format
    print(dtts) #print out converted value. Here is where we would write the conversion back into the original array, overwriting the unix time. 

print('conversion complete')
