Keeping in mind the possibility of storing observations in a database. Which gives potential advantages
in more flexible querying of data, reduced duplication of data, easier realtime metric generation, etc.

Here's an example of a basic API which returns data from a database of observations:

```
    /<stationId>/data ? format=<rinex|rtcm|...> & sampleRate=<duration> & startDatetime=<datetime> & endDatetime=<datetime> (maybe duration instead of end?)
```

The concept of daily, hourly, and highrate RINEX files are semantic and not enforced by the RINEX 
naming standard. A user can request an arbitrary period of data according to the naming standard. 
The standard only gives examples of 15M, 01H, 01D, and 00U (unspecified) - but defines the field as 
being DDU, where DD is file period and U is unit of period (so 99Y, or potentially even Decade, etc.).

That being said, simplified APIs can be provided which follow the daily, hourly, highrate convention. 
Returning the file of given duration for the given datetime - truncated to the relevant fileType duration:
 
```
    /<stationId>/rinex ? datetime=<datetime> & sampleRate=<duration> & fileType=<daily|hourly|highrate>
```

e.g. the following query would return the file "ALIC00AUS_S_20190010100_01H_30S_MO.rnx":

```
    /ALIC00AUS/rinex ? date=2019-01-01T00:01:00Z & sampleRate=30s & fileType=hourly
```
