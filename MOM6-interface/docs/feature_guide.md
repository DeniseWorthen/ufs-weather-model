# mom_cap_outputlog

# Introduction

The tincrement interval (defined in minutes) is used to construct the output filename
the file name must be set as the mid-point of the averaging period via the diagtable
and the output filename timestrings are given by

T - (interval * 60 * increment + interval/2 * 60 * increment )

where T is the time when the file is closed

  00   .   03   .   06   .   09
      1:30 = 6 - (3 + 1:30)
               4:30 = 9 - (3 + 1:30)

  00   .   06   .   12   .   18
      03 = 12 - (6 + 3)
                09 = 18 - (6 + 3)

  00   .   24   .   48   .   72
      12 = 48 - (24 + 12)
                36 = 72 - (24 + 12)

when the model reaches the stop time, any 'pending' output file is closed, and the final
interval output is also closed

                  stop
 18   .   24   .   30
     21 = 30 - (12 + 3)
               03 = 30 - (3)

since both the final interval and the next-to-final interval can be closed at the stop time,
a different log file name is required for the final log file, otherwise the next-to-final
log is overwritten

Depending on configuration, the output file can have an unlimited dimension >0 at creation time.
This necessitates checking for an additional criteria using the filesize at creation. An output
file is declared complete either when the unlimited dimension in the file is >0 or when the unlimited
dimension is >0 and the filesize is larger than the initial size.

When a file is determined to be complete, a log file is recorded containing the forecast hour, the
valid time, the name of the output file and the last completed restart file.

!> @image html logging_diagram1.png "Logging Architecture Diagram" width=50%