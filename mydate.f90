program mydate

    implicit none

    integer, parameter :: EXIT_FAILURE = 1
    integer, parameter :: EXIT_SUCCESS = 0

    character(*), parameter :: rfc_2822_format = "%a, %d %b %Y %H:%M:%S %z"
    character(*), parameter :: TZ_UTC0 = "TZ=UTC0 "
    
    logical :: valid_date = .true.
    logical :: date_date = .false.
    logical :: set_date = .false.
    logical :: ok = .false.
    
    integer :: i = 1
    integer :: option_specified_date = 0
    integer :: when
    
    integer, dimension(8) :: date_time
    integer, dimension(9) :: gmt
    integer, dimension(13) :: file_stat
    
    character(len=100) :: optc = ""
    character(len=32) :: datestr = "" 
    character(len=32) :: batch_file = ""
    character(len=32) :: reference = ""
    character(len=32) :: new_format = ""
    character(len=32) :: my_format = ""
    character(len=32) :: set_datestr = ""
    character(len=32) :: date = ""
    
    character(23), dimension(3) :: rfc_3339_format = "%Y-%m-%d"
    character(23), dimension(5) :: iso_8601_format = "%Y-%m-%d"
    
    rfc_3339_format(2) = "%Y-%m-%d %H:%M:%S%:z"
    rfc_3339_format(3) = "%Y-%m-%d %H:%M:%S.%N%:z"
    
    iso_8601_format(2) = "%Y-%m-%dT%H%:z"
    iso_8601_format(3) = "%Y-%m-%dT%H:%M:%S%:z"
    iso_8601_format(4) = "%Y-%m-%dT%H:%M%:z"
    iso_8601_format(5) = "%Y-%m-%dT%H:%M:%S,%N%:z"

    do
        call get_command_argument(i, optc)
        if (len_trim(optc) == 0) exit
        i = i + 1

        select case (optc)
            case ("--")
                call getarg(i, optc)
                if (i < iargc()) then
                    call getarg(i+1, optc)
                    print *, "date: extra operand '"//trim(optc)//"'"
                    call usage(EXIT_FAILURE)
                else if (len_trim(optc) > 0 .and. optc(1:1) /= "+") then
                    print *, "date: invalid date '"//trim(optc)//"'"
                    call exit(EXIT_FAILURE)
                end if
            case ("-d", "--date")
                if (i > iargc()) then
                    if (optc == "-d") then
                        print "(a41)", "date : option requires an argument -- 'd'" 
                    else
                        print "(a36)", "option '--date' requires an argument" 
                    end if
                    call usage(EXIT_FAILURE)
                end if
                call getarg(i, datestr)
                date_date = .true.
                i = i + 1
            case ("-f", "--file")
                if (i > iargc()) then
                    if (optc == "-f") then
                        print "(a41)", "date : option requires an argument -- 'f'" 
                    else
                        print "(a36)", "option '--file' requires an argument" 
                    end if
                    call usage(EXIT_FAILURE)
                end if
                call getarg(i, batch_file)
                i = i + 1
            case ("-I", "--iso-8601")
                new_format = iso_8601_format(1)
            case ("-r", "--reference")
                if (i > iargc()) then
                    if (optc == "-r") then
                        print "(a41)", "date : option requires an argument -- 'r'" 
                    else
                        print "(a41)", "option '--reference' requires an argument" 
                    end if 
                    call usage(EXIT_FAILURE)
                end if
                call getarg(i, reference)
                i = i + 1
            case ("-R", "--rfc-2822")
                new_format = rfc_2822_format
            case ("-s", "--set")
                if (i > iargc()) then
                    if (optc == "-s") then
                        print "(a41)", "date : option requires an argument -- 's'" 
                    else
                        print "(a35)", "option '--set' requires an argument" 
                    end if 
                    call usage(EXIT_FAILURE)
                end if
                call getarg(i, set_datestr)
                set_date = .true.
                i = i + 1
            case ("-u", "--utc", "--univesrsal")
                if (date_time(4) /= 0) then
                    call get_command(optc)
                    call execute_command_line(TZ_UTC0//optc)
                    call exit()
                end if
            case ("--help")
                call usage(EXIT_SUCCESS)
            case ("--version")
                call version()
            case default
                if (optc(1:7) == "--date=") then
                    datestr = optc(8:)
                else if (optc(1:7) == "--file=") then
                    batch_file = optc(8:)
                else if (optc(1:6) == "--set=") then
                    set_datestr = optc(7:)
                    set_date = .true.
                else if (optc(1:12) == "--reference=") then
                    reference = optc(13:)
                else if (optc(1:11) == "--rfc-3339=") then
                    new_format = rfc_3339_format(rfc_3339_fmt(trim(optc(12:))))
                else if (optc(1:2) == "-I") then
                    new_format = iso_8601_format(iso_8601_fmt(trim(optc(3:))))
                else if (optc(1:11) == "--iso-8601=") then
                    new_format = iso_8601_format(iso_8601_fmt(trim(optc(12:))))
                else if (optc(1:1) == "+") then
                    new_format = optc(2:)
                else
                    call usage(EXIT_FAILURE)
                end if
            end select
            
            if (new_format /= "") then
                if (my_format /= "") then
                    print "(a33)", "multiple output formats specified"
                end if
                my_format = new_format
            end if
        end do
        
        option_specified_date = merge(1, 0, date_date) + merge(1, 0, batch_file /= "") + merge(1, 0, reference /= "")
        
        if (option_specified_date > 1) then
            call usage(EXIT_FAILURE)
            print "(a64)", "the options to specify dates for printing are mutually exclusive"
        end if
        
        if (set_date .and. option_specified_date > 0) then
            print "(a62)", "the options to print and set the time may not be used together"
            call usage(EXIT_FAILURE)
        end if
        
        if (my_format == "") then
            my_format = "%a %b %e %H:%M:%S %Z %Y"
        end if
        
        if (batch_file /= "") then
            call get_command(optc)
            i = index(optc, "./mydate")
            call execute_command_line(optc(1:i-1)//optc(i+8:))
        else
            ok = .true.
            call date_and_time (values=date_time)
            if (option_specified_date > 0 .or. set_date) then
                if (reference /= "") then
                    call stat(reference, file_stat)
                    when = file_stat(10)
                    date = ctime(file_stat(10))
                    call gmtime(when, gmt)
                else
                    if (set_date) then
                        datestr = set_datestr
                        print "(a46)", "date: cannot set date: Operation not permitted"
                    end if
                    when = time()
                    date = fdate()                    
                    call gmtime(when, gmt)
                    valid_date = parse_datetime(when, date, gmt, datestr, date_time)
                end if
            else
                when = time()
                date = fdate()
                call gmtime(when, gmt)
            end if
        gmt(6) = gmt(6) + 1900  ! years since 1900 -> current year
        gmt(5) = gmt(5) + 1     ! months 0..11 -> 1..12
        ok = ok .and. show_date (my_format, date_time, date, gmt, when)
        call exit(merge(EXIT_SUCCESS, EXIT_FAILURE, ok))
    end if

contains

    function parse_datetime (when, date, gmt, datestr, date_time)
        implicit none
        integer, intent(inout) :: when
        character(*), intent(inout) :: date
        integer, dimension(9), intent(inout) :: gmt
        integer, dimension(8), intent(in) :: date_time
        character(*), intent(in) :: datestr
        logical :: parse_datetime
        integer :: year, month, day

        if (datestr == "") then
            date = date(1:11)//"00:00:00"//date(20:)
            gmt(3) = 0
        else if ((datestr(5:5) == "-" .and. datestr(8:8) == "-") .or. (datestr(5:5) == "/" .and. datestr(8:8) == "/")) then
            read(datestr(1:4), '(i4)') year
            read(datestr(6:7), '(i2)') month
            read(datestr(9:10), '(i2)') day
            year = year - 1970
            day = day - 1
            select case (month)
                case (1)
                    when = 0
                case (2)
                    when = 2678400
                case (3)
                    when = 5097600
                case (4)
                    when = 7776000
                case (5)
                    when = 10368000
                case (6)
                    when = 13046400
                case (7)
                    when = 15638400
                case (8)
                    when = 18316800
                case (9)
                    when = 20995200
                case (10)
                    when = 23587200
                case (11)
                    when = 26265600
                case (12)
                    when = 28857600
                case default
                    parse_datetime = .true.
                    return
            end select  
            when = when + (-60)*date_time(4) + 86400*day + 31536000*year
            date = ctime(when)
            call gmtime(when, gmt)
        end if

        parse_datetime = .true.
    end function parse_datetime

    function rfc_3339_fmt(arg)
        implicit none
        character (*), intent(in) :: arg
        integer :: rfc_3339_fmt

        select case (arg)
            case ("date")
                rfc_3339_fmt = 1
            case ("seconds")
                rfc_3339_fmt = 2
            case ("date_time")
                rfc_3339_fmt = 3
            case default
                print *, 'date: “'//arg//'” is an invalid argument for “--rfc-3339”'
                print "(a20)", 'valid arguments are:'
                print "(a10)", '  - “date”'
                print "(a13)", '  - “seconds”'
                print "(a8)" , '  - “ns”'
                call usage(EXIT_FAILURE)
        end select

    end function rfc_3339_fmt

    function iso_8601_fmt(arg)
        implicit none
        character (*), intent(in) :: arg
        integer :: iso_8601_fmt

        select case (arg)
            case ("date")
                iso_8601_fmt = 1
            case ("hours")
                iso_8601_fmt = 2
            case ("minutes")
                iso_8601_fmt = 3
            case ("seconds")
                iso_8601_fmt = 4
            case ("date_time")
                iso_8601_fmt = 5
            case default
                print *, 'date: “'//arg//'” is an invalid argument for “--iso-8061”'
                print "(a20)", 'valid arguments are:'
                print "(a11)", '  - “hours”'
                print "(a13)", '  - “minutes”'
                print "(a10)", '  - “date”'
                print "(a13)", '  - “seconds”'
                print "(a8)" , '  - “ns”'
                call usage(EXIT_FAILURE)
        end select

    end function iso_8601_fmt
 
    function show_date (my_format, date_time, date, gmt, when)
        implicit none
        character(*), intent(in) :: my_format, date
        integer, dimension(8), intent(in) :: date_time
        integer, dimension(9), intent(in) :: gmt
        character(len=4) :: c
        logical :: show_date
        integer, intent(in) :: when
        integer :: i = 1

        do 
            if (i > len_trim(my_format)) exit
            c = my_format(i:i)
            i = i + 1
            if (c == "%") then
                if (my_format(i:i+3) == ":::z") then
                    c = ":::z"
                    i = i + 4
                else if (my_format(i:i+2) == "::z") then
                    c = "::z"
                    i = i + 3
                else if (my_format(i:i+1) == ":z") then
                    c = ":z"
                    i = i + 2
                else 
                    c = my_format(i:i)
                    i = i + 1
                end if
                call parser_format(c, date_time, date, gmt, when)
            else
                write (*,"(a1)",advance="no") c
            end if
        end do
        print *
        show_date = .true.
    end function show_date

    subroutine parser_format (c, date_time, date, gmt, when)
        implicit none
        character(*), intent(in) :: c
        character(len=32), intent(in) :: date
        integer, dimension(8), intent(in) :: date_time
        integer, intent(in) :: when
        integer, dimension(9), intent(in) :: gmt
        character(5) :: zone;
        
        call date_and_time(zone=zone)

        select case (c)
            case ("%")
                write (*,"(a1)",advance="no") "%"
            case ("a")
                write (*,"(a3)",advance="no") date(1:3)
            case ("A")
                call parser_day (date(1:3))
            case ("b")
                write (*,"(a3)",advance="no") date(5:7)
            case ("B")
                call parser_month (date(5:7))
            case ("c")
                write (*,"(a24)",advance="no") date
            case ("C")
                write (*,"(i2.2)",advance="no") gmt(6)/100 + merge(1,0,mod(gmt(1),100)>0)
            case ("d")
                write (*,"(i2.2)",advance="no") gmt(4)
            case ("D")
                write (*,"(i2.2,a1,i2.2,a1,a2)",advance="no") gmt(4), "/", gmt(5), "/", date(23:24)
            case ("e")
                write (*,"(a2)",advance="no") date(9:10)
            case ("F")
                write (*,"(i4.4,a1,i2.2,a1,i2.2)",advance="no") gmt(6), "-", gmt(5), "-", gmt(4)
            case ("g")
                write (*,"(a2)",advance="no") date(23:24)
            case ("G")
                write (*,"(a4)",advance="no") date(21:24)
            case ("h")
                write (*,"(a3)",advance="no") date(5:7)
            case ("H")
                write (*,"(a2)",advance="no") date(12:13)
            case ("I")
                write (*,"(i2.2)",advance="no") mod(gmt(3),12) + merge(12,0,mod(gmt(3),12)==0)
            case ("j")
                write (*,"(i3.3)",advance="no") gmt(8) + 1
            case ("k")
                write (*,"(i2)",advance="no") gmt(3)
            case ("l")
                write (*,"(i2)",advance="no") mod(gmt(3),12) + merge(12,0,mod(gmt(3),12)==0)
            case ("m")
                write (*,"(i2.2)",advance="no") gmt(5)
            case ("M")
                write (*,"(a2)",advance="no") date(15:16)
            case ("n")
                print *
            case ("N")
                write (*,"(i3.3,a6)",advance="no") date_time(8), "000000"
            case ("p")
                ! 
            case ("P")
                !
            case ("r")
                write (*,"(i2.2)",advance="no") mod(gmt(3),12) + merge(12,0,mod(gmt(3),12)==0)
                write (*,"(a1,i2.2,a1,i2.2)",advance="no") ":", gmt(2), ":", gmt(7)
            case ("R")
                write (*,"(a5)",advance="no") date(12:16)
            case ("s")
                write (*,"(i10)",advance="no") when
            case ("S")
                write (*,"(a2)",advance="no") date(18:19)
            case ("t")
                write (*,"(a1)",advance="no") achar(9)
            case ("T")
                write (*,"(a8)",advance="no") date(12:19)
            case ("u")
                write (*,"(i1)",advance="no") gmt(7) + merge(7,0,gmt(7)==0)
            case ("U")
                write (*,"(i2.2)",advance="no") gmt(8)/7
            case ("V")
                write (*,"(i2.2)",advance="no") gmt(8)/7+1
            case ("w")
                write (*,"(i1)",advance="no") gmt(7)
            case ("W")
                write (*,"(i2.2)",advance="no") gmt(8)/7
            case ("x")
                write (*,"(i2.2,a1,i2.2,a1,a2)",advance="no") gmt(4), "/", gmt(5), "/", date(23:24)
            case ("X")
                write (*,"(a8)",advance="no") date(12:19)
            case ("y")
                write (*,"(a8)",advance="no") date(23:24)
            case ("Y")
                write (*,"(a4)",advance="no") date(21:24)
            case ("z")
                write (*,"(a5)",advance="no") zone
            case (":z")
                write (*,"(a6)",advance="no") zone(1:3)//":"//zone(4:)
            case ("::z")
                write (*,"(a9)",advance="no") zone(1:3)//":"//zone(4:)//":00"
            case (":::z")
                write (*,"(a3)",advance="no") zone(1:3)
                if (zone(4:5) /= "00") then
                    write (*,"(a3)",advance="no") ":"//zone(4:)
                end if
            case ("Z")
                if (zone == "+0000") then
                    write (*,"(a3)",advance="no") "UTC"
                else
                    write (*,"(a3)",advance="no") zone(1:3)
                end if
            case default
                write (*, "(a2)", advance="no") "%"//c
        end select

    end subroutine parser_format

    subroutine parser_month (month)
        implicit none
        character(len=3),intent(in) :: month

        select case (month)
            case ("Jan")
                write (*,"(a7)",advance="no") "January"
            case ("Feb")
                write (*,"(a8)",advance="no") "February"
            case ("Mar")
                write (*,"(a5)",advance="no") "March"
            case ("Apr")
                write (*,"(a5)",advance="no") "April"
            case ("May")
                write (*,"(a3)",advance="no") "May"
            case ("Jun")
                write (*,"(a4)",advance="no") "June"
            case ("Jul")
                write (*,"(a4)",advance="no") "July"
            case ("Aug")
                write (*,"(a6)",advance="no") "August"
            case ("Sep")
                write (*,"(a9)",advance="no") "September"
            case ("Oct")
                write (*,"(a7)",advance="no") "October"
            case ("Nov")
                write (*,"(a8)",advance="no") "November"
            case ("Dec")
                write (*,"(a8)",advance="no") "December"
        end select

    end subroutine parser_month

    subroutine parser_day (day)
        implicit none
        character(len=3), intent(in) :: day
    
        select case (day)
            case ("Mon")
                write (*,"(a6)",advance="no") "Monday"
            case ("Tue")
                write (*,"(a7)",advance="no") "Tuesday"
            case ("Wed")
                write (*,"(a9)",advance="no") "Wednesday"
            case ("Thu")
                write (*,"(a8)",advance="no") "Thursday"
            case ("Fri")
                write (*,"(a6)",advance="no") "Friday"
            case ("Sat")
                write (*,"(a8)",advance="no") "Saturday"
            case ("Sun")
                write (*,"(a6)",advance="no") "Sunday"
            end select
            
    end subroutine parser_day

    subroutine version ()
        print "(a25)", "date (GNU coreutils) 8.30"
        print "(a49)", "Copyright (C) 2018 Free Software Foundation, Inc."
        print "(a79)", "License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>."
        print "(a66)", "This is free software: you are free to change and redistribute it."
        print "(a53)", "There is NO WARRANTY, to the extent permitted by law."
        print *
        print "(a27)", "Written by David MacKenzie."
        
        call exit(EXIT_SUCCESS)
    end subroutine version
        
    subroutine usage (status)
        implicit none
        integer, intent(in) :: status
        
        if (status /= EXIT_SUCCESS) then
            write (*, *) "Try './mydate --help' for more information."
        else
            print "(a33)", "Usage: date [OPTION]... [+FORMAT]"
            print "(a58)", "  or:  date [-u|--utc|--universal] [MMDDhhmm[[CC]YY][.ss]]"
            print "(a69)", "Display the current time in the given FORMAT, or set the system date."
            print *
            print "(a72)", "Mandatory arguments to long options are mandatory for short options too."
            print "(a72)", "  -d, --date=STRING          display time described by STRING, not 'now'"
            print "(a72)", "  -f, --file=DATEFILE        like --date; once for each line of DATEFILE"
            print "(a65)", "  -I[FMT], --iso-8601[=FMT]  output date/time in ISO 8601 format."
            print "(a70)", "                               FMT='date' for date only (the default),"
            print "(a69)", "                               'hours', 'minutes', 'seconds', or 'ns'"
            print "(a76)", "                               for date and time to the indicated precision."
            print "(a64)", "                               Example: 2006-08-14T02:34:56-0600"
            print "(a69)", "  -R, --rfc-2822             output date and time in RFC 2822 format."
            print "(a71)", "                               Example: Mon, 14 Aug 2006 02:34:56 -0600"
            print "(a65)", "      --rfc-3339=FMT         output date/time in RFC 3339 format."
            print "(a61)", "                               FMT='date', 'seconds', or 'ns'"
            print "(a76)", "                               for date and time to the indicated precision."
            print "(a65)", "                               Example: 2006-08-14 02:34:56-06:00"
            print "(a71)", "  -r, --reference=FILE       display the last modification time of FILE"
            print "(a57)", "  -s, --set=STRING           set time described by STRING"
            print "(a74)", "  -u, --utc, --universal     print or set Coordinated Universal Time (UTC)"
            print "(a43)", "      --help     display this help and exit"
            print "(a52)", "      --version  output version information and exit"
            print *
            print "(a55)", "FORMAT controls the output.  Interpreted sequences are:"
            print *
            print "(a18)", "  %%   a literal %"
            print "(a52)", "  %a   locale's abbreviated weekday name (e.g., Sun)"
            print "(a48)", "  %A   locale's full weekday name (e.g., Sunday)"
            print "(a50)", "  %b   locale's abbreviated month name (e.g., Jan)"
            print "(a47)", "  %B   locale's full month name (e.g., January)"
            print "(a62)", "  %c   locale's date and time (e.g., Thu Mar  3 23:05:25 2005)"
            print "(a63)", "  %C   century; like %Y, except omit last two digits (e.g., 20)"
            print "(a30)", "  %d   day of month (e.g., 01)"
            print "(a29)", "  %D   date; same as %m/%d/%y"
            print "(a46)", "  %e   day of month, space padded; same as %_d"
            print "(a34)", "  %F   full date; same as %Y-%m-%d"
            print "(a58)", "  %g   last two digits of year of ISO week number (see %G)"
            print "(a69)", "  %G   year of ISO week number (see %V); normally useful only with %V"
            print "(a17)", "  %h   same as %b"
            print "(a20)", "  %H   hour (00..23)"
            print "(a20)", "  %I   hour (01..12)"
            print "(a29)", "  %j   day of year (001..366)"
            print "(a47)", "  %k   hour, space padded ( 0..23); same as %_H"
            print "(a47)", "  %l   hour, space padded ( 1..12); same as %_I"
            print "(a21)", "  %m   month (01..12)"
            print "(a22)", "  %M   minute (00..59)"
            print "(a16)", "  %n   a newline"
            print "(a41)", "  %N   nanoseconds (000000000..999999999)"
            print "(a65)", "  %p   locale's equivalent of either AM or PM; blank if not known"
            print "(a30)", "  %P   like %p, but lower case"
            print "(a54)", "  %r   locale's 12-hour clock time (e.g., 11:11:04 PM)"
            print "(a45)", "  %R   24-hour hour and minute; same as %H:%M"
            print "(a44)", "  %s   seconds since 1970-01-01 00:00:00 UTC"
            print "(a22)", "  %S   second (00..60)"
            print "(a12)", "  %t   a tab"
            print "(a29)", "  %T   time; same as %H:%M:%S"
            print "(a38)", "  %u   day of week (1..7); 1 is Monday"
            print "(a69)", "  %U   week number of year, with Sunday as first day of week (00..53)"
            print "(a65)", "  %V   ISO week number, with Monday as first day of week (01..53)"
            print "(a38)", "  %w   day of week (0..6); 0 is Sunday"
            print "(a69)", "  %W   week number of year, with Monday as first day of week (00..53)"
            print "(a52)", "  %x   locale's date representation (e.g., 12/31/99)"
            print "(a52)", "  %X   locale's time representation (e.g., 23:13:48)"
            print "(a39)", "  %y   last two digits of year (00..99)"
            print "(a11)", "  %Y   year"
            print "(a44)", "  %z   +hhmm numeric time zone (e.g., -0400)"
            print "(a46)", "  %:z  +hh:mm numeric time zone (e.g., -04:00)"
            print "(a53)", "  %::z  +hh:mm:ss numeric time zone (e.g., -04:00:00)"
            print "(a76)", "  %:::z  numeric time zone with : to necessary precision (e.g., -04, +05:30)"
            print "(a52)", "  %Z   alphabetic time zone abbreviation (e.g., EDT)"
            print *
            print "(a49)", "By default, date pads numeric fields with zeroes."
            print "(a44)", "The following optional flags may follow '%':"
            print *
            print "(a34)", "  -  (hyphen) do not pad the field"
            print "(a33)", "  _  (underscore) pad with spaces"
            print "(a26)", "  0  (zero) pad with zeros"
            print "(a31)", "  ^  use upper case if possible"
            print "(a34)", "  #  use opposite case if possible"
            print *
            print "(a67)", "After any flags comes an optional field width, as a decimal number;"
            print "(a42)", "then an optional modifier, which is either"
            print "(a64)", "E to use the locale's alternate representations if available, or"
            print "(a61)", "O to use the locale's alternate numeric symbols if available."
            print *
            print "(a9)", "Examples:"
            print "(a58)", "Convert seconds since the epoch (1970-01-01 UTC) to a date"
            print "(a29)", "  $ date --date='@2147483647'"
            print *
            print "(a70)", "Show the time on the west coast of the US (use tzselect(1) to find TZ)"
            print "(a33)", "  $ TZ='America/Los_Angeles' date"
            print *
            print "(a67)", "Show the local time for 9AM next Friday on the west coast of the US"
            print "(a59)", "  $ date --date='TZ=""America/Los_Angeles"" 09:00 next Fri'"
            print *
            print "(a67)", "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
            print "(a69)", "Report date translation bugs to <http://translationproject.org/team/>"
            print "(a67)", "Full documentation at: <http://www.gnu.org/software/coreutils/date>"
            print "(a60)", "or available locally via: info '(coreutils) date invocation'"     
        end if

        call exit(status)
    end subroutine usage
    
end program mydate