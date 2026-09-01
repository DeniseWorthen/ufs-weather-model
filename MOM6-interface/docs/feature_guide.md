# MOM6 Output Logging User Guide

# Introduction

The MOM6 output logging feature is designed to track the completion of MOM6 history and restart files during a model run.
This feature is specific to UWM operational requirements and configurations (eg specific output frequencies in hours) and may
break if used outside the scope of intended use.

The feature is enabled by adding a namelist to the model `input.nml` which can be used to define the file names, frequencies
and types (snapshot or averages) which are to be tracked. The user is responsible for ensuring that these settings match the
contents of the `diag_table` in use by the model.

From the user-configuration, one or more Alarms are enabled at the desired frequencies. A filename is constructed referencing
the model time and the initial state of the file is recorded. Depending on configuration, an output file can have an unlimited
dimension greater than zero at creation time. This necessitates checking for an additional criteria using the filesize at creation.
Depending on the characteristics of the file at creation, the criteria to declare a file complete is set.

The file state will be checked at each suceeding ModelAdvance until the appropriate completion criteria is met: either when the
unlimited dimension in the file is greater than zero or when the unlimited dimension is greater than zero and the filesize is
larger than the initial size. When a file is determined to be complete, a log file is recorded containing the forecast hour, the valid time, the name of the output file and the last completed restart file.

Depending on configuration, the final file and the next-to-final file can be closed at the stop time. Therefore a different
log file name is required for the final log file, otherwise the next-to-final log is overwritten

## Feature Configuration

The output log feature is enabled with a `MOM_outputlog_nml` namelist added to the `input.nml`. For example, the following values
will set the logging feature to track 6-hourly files, which (using the `diag_table`) have a filename prefix of `ocn` and are defined
as time-averaged values. No debug information will be added to the stdout file.

```fortran
&MOM_outputlog_nml
  outputlog_fh = 6
  outputlog_fnameprefix = 'ocn'
  outputlog_treduce = 'average'
  outputlog_debug = .false.
/
```

The following rules apply the namelist options for output logging.

### Logging Frequency

File tracking can be enabled for 1,3,6 or 24 hourly files only. Multiple frequencies can be requested and the listed order is
immaterial. However, each logging frequency must be uniquely defined. For example, 6-hourly average files and 6-hourly snapshot
files are not allowed but 6-hourly average and 3-hourly snapshot files are.

### Filename Prefix

If a single frequency is requested, no filename prefix is required. A default prefix of `ocn_` will be used. It is the user's
responsibility that the default matches the specification in the `diag_table`. Otherwise, they must set the actual filename
prefix called for in the `diag_table`.

If more than a single frequency is requested, the user must provide filename prefixes for all frequencies (again ensuring matches
to the `diag_table`.

The filename prefix can have a maximum length of 12 (not including the trailing underscore, which will be appended).

### File Time Reduction

Either instantaneous (snapshot) files or time averaged files are supported. These are specified by `treduce` settings of `none` and
`average`, respectfully.



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

Depending on configuration, the output file can have an unlimited dimension >0 at creation time.
This necessitates checking for an additional criteria using the filesize at creation. An output
file is declared complete either when the unlimited dimension in the file is >0 or when the unlimited
dimension is >0 and the filesize is larger than the initial size.

When a file is determined to be complete, a log file is recorded containing the forecast hour, the
valid time, the name of the output file and the last completed restart file.

!> @image html logging_diagram1.png "Logging Architecture Diagram" width=50%