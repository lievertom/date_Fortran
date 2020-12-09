program mydate

    implicit none
    
    type when_type
        integer :: sec
        character(len=5) :: zone
        character(len=32) :: date = ""
        integer, dimension(9) :: gmt
        integer, dimension(8) :: date_time
    end type when_type
    
    type(when_type) :: when

    integer, parameter :: EXIT_FAILURE = 1
    integer, parameter :: EXIT_SUCCESS = 0
    
    character(*), parameter :: rfc_2822_format = "%a, %d %b %Y %H:%M:%S %z"
    character(*), parameter :: TZ_UTC0 = "TZ=UTC0 "
    
    logical :: valid_date = .true.
    logical :: date_date = .false.
    logical :: set_date = .false.
    logical :: ok = .true.
    
    integer :: i = 1
    integer :: option_specified_date = 0
    integer :: set_status
    
    integer, dimension(13) :: file_stat
    
    character(len=100) :: optc = ""
    character(len=32) :: datestr = "" 
    character(len=32) :: batch_file = ""
    character(len=32) :: reference = ""
    character(len=32) :: new_format = ""
    character(len=32) :: my_format = ""
    character(len=32) :: set_datestr = ""
    
    character(23), dimension(3) :: rfc_3339_format = "%Y-%m-%d"
    character(23), dimension(5) :: iso_8601_format = "%Y-%m-%d"
    
    rfc_3339_format(2) = "%Y-%m-%d %H:%M:%S%:z"
    rfc_3339_format(3) = "%Y-%m-%d %H:%M:%S.%N%:z"
    
    iso_8601_format(2) = "%Y-%m-%dT%H%:z"
    iso_8601_format(3) = "%Y-%m-%dT%H:%M:%S%:z"
    iso_8601_format(4) = "%Y-%m-%dT%H:%M%:z"
    iso_8601_format(5) = "%Y-%m-%dT%H:%M:%S,%N%:z"

    when%sec = time()
    call fdate(when%date)
    call gmtime(when%sec, when%gmt)
    call date_and_time (values=when%date_time, zone=when%zone)

    do
        call get_command_argument(i, optc)
        if (len_trim(optc) == 0) exit
        i = i + 1

        select case (optc)
            case ("--")
                call getarg(i, optc)
                if (i < iargc()) then
                    call getarg(i+1, optc)
                    print "(A)", "date: extra operand '"//trim(optc)//"'"
                    call usage(EXIT_FAILURE)
                else if (len_trim(optc) > 0 .and. optc(1:1) /= "+") then
                    print "(A)", "date: invalid date '"//trim(optc)//"'"
                    call exit(EXIT_FAILURE)
                end if
            case ("-d", "--date")
                if (i > iargc()) then
                    if (optc == "-d") then
                        print "(A)", "date: option requires an argument -- 'd'" 
                    else
                        print "(A)", "date: option '--date' requires an argument" 
                    end if
                    call usage(EXIT_FAILURE)
                end if
                call getarg(i, datestr)
                date_date = .true.
                i = i + 1
            case ("-f", "--file")
                if (i > iargc()) then
                    if (optc == "-f") then
                        print "(A)", "date: option requires an argument -- 'f'" 
                    else
                        print "(A)", "date: option '--file' requires an argument" 
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
                        print "(A)", "date: option requires an argument -- 'r'" 
                    else
                        print "(A)", "date: option '--reference' requires an argument" 
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
                        print "(A)", "date: option requires an argument -- 's'" 
                    else
                        print "(A)", "date: option '--set' requires an argument" 
                    end if 
                    call usage(EXIT_FAILURE)
                end if
                call getarg(i, set_datestr)
                set_date = .true.
                i = i + 1
            case ("-u", "--utc", "--univesrsal")
                if (when%zone /= "+0000") then
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
                else if (optc(1:2) == "--") then
                    print "(A)", "date: unrecognized option '"//optc//"'"
                    call usage(EXIT_FAILURE)
                else if (optc(1:1) == "-" .and. len_trim(optc) > 1) then
                    print "(A)", "date: invalid option -- '"//optc(2:2)//"'"
                    call usage(EXIT_FAILURE)
                else if (i <= iargc()) then
                    call getarg(i, optc)
                    print "(A)", "date: extra operand '"//trim(optc)//"'"
                    call usage(EXIT_FAILURE)
                else
                    print "(A)", "date: invalid date '"//trim(optc)//"'"
                    call exit(EXIT_FAILURE)
                end if
        end select
            
        if (new_format /= "") then
            if (my_format /= "") then
                print "(A)", "multiple output formats specified"
            end if
            my_format = new_format
        end if
    end do
        
    option_specified_date = merge(1, 0, date_date) + merge(1, 0, batch_file /= "") + merge(1, 0, reference /= "")
    
    if (option_specified_date > 1) then
        call usage(EXIT_FAILURE)
        print "(A)", "the options to specify dates for printing are mutually exclusive"
    end if
    
    if (set_date .and. option_specified_date > 0) then
        print "(A)", "the options to print and set the time may not be used together"
        call usage(EXIT_FAILURE)
    end if
    
    if (my_format == "") then
        my_format = "%a %b %e %H:%M:%S %Z %Y"
    end if
    
    if (batch_file /= "") then
        ok = batch_convert (batch_file, my_format, when)
    else
        if (option_specified_date > 0 .or. set_date) then
            if (reference /= "") then
                call stat(reference, file_stat)
                when%sec = file_stat(10)
                When%date = ctime(file_stat(10))
                call gmtime(file_stat(10), when%gmt)
            else
                if (set_datestr /= "") then
                    datestr = set_datestr
                end if
                valid_date = parse_datetime(datestr, when)
            end if
        end if
        if (.not.valid_date) then
            print "(A)", "date: invalid date '"//trim(datestr)//"'"
            call exit(EXIT_FAILURE)
        end if
        if (set_date) then  !simulates set_date return
            set_status = chmod("/dev/log", "u+r")  
            if (set_status /= 0) then  !check permission
                print "(a46)", "date: cannot set date: Operation not permitted"
            end if
        end if
        ok = ok .and. show_date (my_format, when)
    end if
    call exit(merge(EXIT_SUCCESS, EXIT_FAILURE, ok))
    
    contains

    function extract_date (year, month,day, i, j, datestr)
        integer, intent(inout) :: year 
        integer, intent(inout) :: month 
        integer, intent(inout) :: day
        integer, intent(in) :: i 
        integer, intent(inout) :: j
        character(*), intent(in) :: datestr
        integer :: error
        logical :: extract_date
        
        j = j + i
        if (i < 2) then
            extract_date = .false.
            return
        else if (i == j) then
            year = 0
            read(datestr(1:i-1), '(i5)', err=20, iostat=error) month 
            read(datestr(i+1:), '(i5)', err=20, iostat=error) day
        else
            read(datestr(1:i-1), '(i5)', err=20, iostat=error) year 
            read(datestr(i+1:j-1), '(i5)', err=20, iostat=error) month 
            read(datestr(j+1:), '(i5)', err=20, iostat=error) day
        end if

        if (year < 0 .or. day < 1 .or. day > 31) then
            extract_date = .false.
            return
        end if

        if (year < 69) then
            year = year + 2000
        else if (year < 100) then
            year = year + 1900
        end if

20      if (error /= 0) then
            extract_date = .false.
        else
            extract_date = .true.
        end if

    end function extract_date

    function parse_datetime (datestr, when)
        implicit none
        character(*), intent(in) :: datestr
        type(when_type), intent(inout) :: when
        logical :: parse_datetime
        integer :: year, month, day, i, j
        character(len=32) :: datesub

        select case (datestr)
            case("", "-")
                when%date = when%date(1:11)//"00:00:00"//when%date(20:)
                when%gmt(3) = 0
                parse_datetime = .true.
                return
            case default
                i = index(datestr, "-")
                if (i > 0) then
                    datesub = datestr(i+1:)
                    j = index(datesub, "-")
                    parse_datetime = extract_date(year, month,day, i, j, datestr)
                else
                    i = index(datestr, "/")
                    if (i > 0) then
                        datesub = datestr(i+1:)
                        j = index(datesub, "/")
                        parse_datetime = extract_date(year, month, day, i, j, datestr)
                    else
                        parse_datetime = .false.
                    end if   
                end if
        end select

        if(.not.parse_datetime) return

        select case (month)
            case (1)
                when%sec = 0
            case (2)
                if ((day > 28 .and. mod(year, 4) > 0) .or. day > 29) then
                    parse_datetime = .false.
                    return
                end if
                when%sec = 2678400
            case (3)
                when%sec = 5097600
            case (4)
                if (day > 30) then
                    parse_datetime = .false.
                    return
                end if
                when%sec = 7776000
            case (5)
                when%sec = 10368000
            case (6)
                if (day > 30) then
                    parse_datetime = .false.
                    return
                end if
                when%sec = 13046400
            case (7)
                when%sec = 15638400
            case (8)
                when%sec = 18316800
            case (9)
                if (day > 30) then
                    parse_datetime = .false.
                    return
                end if
                when%sec = 20995200
            case (10)
                when%sec = 23587200
            case (11)
                if (day > 30) then
                    parse_datetime = .false.
                    return
                end if
                when%sec = 26265600
            case (12)
                when%sec = 28857600
            case default
                parse_datetime = .false.
                return
        end select

        year = year - 1970
        day = day - 1

        if (year >= 0) then
            day = day + (year+2)/4 - merge(1,0,modulo(year+2,4)==0 .and. month < 3)
        else
            day = day + (year-2)/4 + merge(1,0,modulo(year+2,4)==0 .and. month > 2)
        end if 

        when%sec = when%sec + (-60)*when%date_time(4) + 86400*day + 31536000*year
        when%date = ctime(when%sec)
        call gmtime(when%sec, when%gmt)

        parse_datetime = .true.
    end function parse_datetime

    function batch_convert (input_filename, my_format, when)
        implicit none
        character(*), intent(in) :: input_filename
        character(*), intent(in) :: my_format
        type(when_type), intent(inout) :: when
        logical :: batch_convert, valid_date
        character(len=32) :: line = ""
        integer :: error, in_stream
        
        batch_convert = .true.

        if (input_filename == "-") then
            in_stream = 5
        else
            in_stream = 1
            open(unit=in_stream, file=input_filename, err=100, status="old",action="read", iostat=error)
100         if(error > 0) then
                print "(A)",  "date: "//trim(input_filename)//": No such file or directory"
                call exit(EXIT_FAILURE)
            end if
        end if
        do
            read(in_stream, *, iostat = error) line
            if (error /= 0) exit
            valid_date = parse_datetime (line, when)
            if (.not.valid_date) then
                print "(A)", "date: invalid date '"//trim(line)//"'"
                batch_convert = .false.
            else 
                batch_convert =  show_date (my_format, when)
            end if
        end do
        
        if (in_stream == 1) then
            close(1)
        end if

    end function batch_convert

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
                print "(A)", 'date: “'//arg//'” is an invalid argument for “--rfc-3339”'
                print "(A)", 'valid arguments are:'
                print "(A)", '  - “date”'
                print "(A)", '  - “seconds”'
                print "(A)", '  - “ns”'
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
                print "(A)", 'date: “'//arg//'” is an invalid argument for “--iso-8061”'
                print "(A)", 'valid arguments are:'
                print "(A)", '  - “hours”'
                print "(A)", '  - “minutes”'
                print "(A)", '  - “date”'
                print "(A)", '  - “seconds”'
                print "(A)", '  - “ns”'
                call usage(EXIT_FAILURE)
        end select

    end function iso_8601_fmt
 
    function show_date (my_format, when)
        implicit none
        character(*), intent(in) :: my_format
        type(when_type), intent(inout) :: when
        logical :: show_date  
        character(len=4) :: c
        integer :: i
        i = 1
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
                call parser_format(c, when)
            else
                write (*,"(a1)",advance="no") c
            end if
        end do
        print *
        show_date = .true.
    end function show_date

    subroutine parser_format (c, when)
        implicit none
        character(*), intent(in) :: c
        type(when_type), intent(inout) :: when
        
        when%gmt(6) = when%gmt(6) + 1900  ! years since 1900 -> current year
        when%gmt(5) = when%gmt(5) + 1     ! months 0..11 -> 1..12

        select case (c)
            case ("%")
                write (*,"(a1)",advance="no") "%"
            case ("a")
                write (*,"(a3)",advance="no") when%date(1:3)
            case ("A")
                call parser_day (when%date(1:3))
            case ("b")
                write (*,"(a3)",advance="no") when%date(5:7)
            case ("B")
                call parser_month (when%date(5:7))
            case ("c")
                write (*,"(a24)",advance="no") when%date
            case ("C")
                write (*,"(i2.2)",advance="no") when%gmt(6)/100 + merge(1,0,mod(when%gmt(1),100)>0)
            case ("d")
                write (*,"(i2.2)",advance="no") when%gmt(4)
            case ("D")
                write (*,"(i2.2,a1,i2.2,a1,a2)",advance="no") when%gmt(4), "/", when%gmt(5), "/", when%date(23:24)
            case ("e")
                write (*,"(a2)",advance="no") when%date(9:10)
            case ("F")
                write (*,"(i4.4,a1,i2.2,a1,i2.2)",advance="no") when%gmt(6), "-", when%gmt(5), "-", when%gmt(4)
            case ("g")
                write (*,"(a2)",advance="no") when%date(23:24)
            case ("G")
                write (*,"(a4)",advance="no") when%date(21:24)
            case ("h")
                write (*,"(a3)",advance="no") when%date(5:7)
            case ("H")
                write (*,"(a2)",advance="no") when%date(12:13)
            case ("I")
                write (*,"(i2.2)",advance="no") mod(when%gmt(3),12) + merge(12,0,mod(when%gmt(3),12)==0)
            case ("j")
                write (*,"(i3.3)",advance="no") when%gmt(8) + 1
            case ("k")
                write (*,"(i2)",advance="no") when%gmt(3)
            case ("l")
                write (*,"(i2)",advance="no") mod(when%gmt(3),12) + merge(12,0,mod(when%gmt(3),12)==0)
            case ("m")
                write (*,"(i2.2)",advance="no") when%gmt(5)
            case ("M")
                write (*,"(a2)",advance="no") when%date(15:16)
            case ("n")
                print *
            case ("N")
                write (*,"(i3.3,a6)",advance="no") when%date_time(8), "000000"
            case ("p")
                ! 
            case ("P")
                !
            case ("r")
                write (*,"(i2.2)",advance="no") mod(when%gmt(3),12) + merge(12,0,mod(when%gmt(3),12)==0)
                write (*,"(a1,i2.2,a1,i2.2)",advance="no") ":", when%gmt(2), ":", when%gmt(7)
            case ("R")
                write (*,"(a5)",advance="no") when%date(12:16)
            case ("s")
                write (*,"(i10)",advance="no") when
            case ("S")
                write (*,"(a2)",advance="no") when%date(18:19)
            case ("t")
                write (*,"(a1)",advance="no") achar(9)
            case ("T")
                write (*,"(a8)",advance="no") when%date(12:19)
            case ("u")
                write (*,"(i1)",advance="no") when%gmt(7) + merge(7,0,when%gmt(7)==0)
            case ("U")
                write (*,"(i2.2)",advance="no") when%gmt(8)/7
            case ("V")
                write (*,"(i2.2)",advance="no") when%gmt(8)/7+1
            case ("w")
                write (*,"(i1)",advance="no") when%gmt(7)
            case ("W")
                write (*,"(i2.2)",advance="no") when%gmt(8)/7
            case ("x")
                write (*,"(i2.2,a1,i2.2,a1,a2)",advance="no") when%gmt(4), "/", when%gmt(5), "/", when%date(23:24)
            case ("X")
                write (*,"(a8)",advance="no") when%date(12:19)
            case ("y")
                write (*,"(a8)",advance="no") when%date(23:24)
            case ("Y")
                write (*,"(a4)",advance="no") when%date(21:24)
            case ("z")
                write (*,"(a5)",advance="no") when%zone
            case (":z")
                write (*,"(a6)",advance="no") when%zone(1:3)//":"//when%zone(4:)
            case ("::z")
                write (*,"(a9)",advance="no") when%zone(1:3)//":"//when%zone(4:)//":00"
            case (":::z")
                write (*,"(a3)",advance="no") when%zone(1:3)
                if (when%zone(4:5) /= "00") then
                    write (*,"(a3)",advance="no") ":"//when%zone(4:)
                end if
            case ("Z")
                if (when%zone == "+0000") then
                    write (*,"(a3)",advance="no") "UTC"
                else
                    write (*,"(a3)",advance="no") when%zone(1:3)
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
        print "(A)", "date (GNU coreutils) 8.30"
        print "(A)", "Copyright (C) 2018 Free Software Foundation, Inc."
        print "(A)", "License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>."
        print "(A)", "This is free software: you are free to change and redistribute it."
        print "(A)", "There is NO WARRANTY, to the extent permitted by law."
        print *
        print "(A)", "Written by David MacKenzie."
        
        call exit(EXIT_SUCCESS)
    end subroutine version
        
    subroutine usage (status)
        implicit none
        integer, intent(in) :: status
        
        if (status /= EXIT_SUCCESS) then
            print "(A)", "Try 'date --help' for more information."
        else
            print "(A)", "Usage: date [OPTION]... [+FORMAT]"
            print "(A)", "  or:  date [-u|--utc|--universal] [MMDDhhmm[[CC]YY][.ss]]"
            print "(A)", "Display the current time in the given FORMAT, or set the system date."
            print *
            print "(A)", "Mandatory arguments to long options are mandatory for short options too."
            print "(A)", "  -d, --date=STRING          display time described by STRING, not 'now'"
            print "(A)", "  -f, --file=DATEFILE        like --date; once for each line of DATEFILE"
            print "(A)", "  -I[FMT], --iso-8601[=FMT]  output date/time in ISO 8601 format."
            print "(A)", "                               FMT='date' for date only (the default),"
            print "(A)", "                               'hours', 'minutes', 'seconds', or 'ns'"
            print "(A)", "                               for date and time to the indicated precision."
            print "(A)", "                               Example: 2006-08-14T02:34:56-0600"
            print "(A)", "  -R, --rfc-2822             output date and time in RFC 2822 format."
            print "(A)", "                               Example: Mon, 14 Aug 2006 02:34:56 -0600"
            print "(A)", "      --rfc-3339=FMT         output date/time in RFC 3339 format."
            print "(A)", "                               FMT='date', 'seconds', or 'ns'"
            print "(A)", "                               for date and time to the indicated precision."
            print "(A)", "                               Example: 2006-08-14 02:34:56-06:00"
            print "(A)", "  -r, --reference=FILE       display the last modification time of FILE"
            print "(A)", "  -s, --set=STRING           set time described by STRING"
            print "(A)", "  -u, --utc, --universal     print or set Coordinated Universal Time (UTC)"
            print "(A)", "      --help     display this help and exit"
            print "(A)", "      --version  output version information and exit"
            print *
            print "(A)", "FORMAT controls the output.  Interpreted sequences are:"
            print *
            print "(A)", "  %%   a literal %"
            print "(A)", "  %a   locale's abbreviated weekday name (e.g., Sun)"
            print "(A)", "  %A   locale's full weekday name (e.g., Sunday)"
            print "(A)", "  %b   locale's abbreviated month name (e.g., Jan)"
            print "(A)", "  %B   locale's full month name (e.g., January)"
            print "(A)", "  %c   locale's date and time (e.g., Thu Mar  3 23:05:25 2005)"
            print "(A)", "  %C   century; like %Y, except omit last two digits (e.g., 20)"
            print "(A)", "  %d   day of month (e.g., 01)"
            print "(A)", "  %D   date; same as %m/%d/%y"
            print "(A)", "  %e   day of month, space padded; same as %_d"
            print "(A)", "  %F   full date; same as %Y-%m-%d"
            print "(A)", "  %g   last two digits of year of ISO week number (see %G)"
            print "(A)", "  %G   year of ISO week number (see %V); normally useful only with %V"
            print "(A)", "  %h   same as %b"
            print "(A)", "  %H   hour (00..23)"
            print "(A)", "  %I   hour (01..12)"
            print "(A)", "  %j   day of year (001..366)"
            print "(A)", "  %k   hour, space padded ( 0..23); same as %_H"
            print "(A)", "  %l   hour, space padded ( 1..12); same as %_I"
            print "(A)", "  %m   month (01..12)"
            print "(A)", "  %M   minute (00..59)"
            print "(A)", "  %n   a newline"
            print "(A)", "  %N   nanoseconds (000000000..999999999)"
            print "(A)", "  %p   locale's equivalent of either AM or PM; blank if not known"
            print "(A)", "  %P   like %p, but lower case"
            print "(A)", "  %r   locale's 12-hour clock time (e.g., 11:11:04 PM)"
            print "(A)", "  %R   24-hour hour and minute; same as %H:%M"
            print "(A)", "  %s   seconds since 1970-01-01 00:00:00 UTC"
            print "(A)", "  %S   second (00..60)"
            print "(A)", "  %t   a tab"
            print "(A)", "  %T   time; same as %H:%M:%S"
            print "(A)", "  %u   day of week (1..7); 1 is Monday"
            print "(A)", "  %U   week number of year, with Sunday as first day of week (00..53)"
            print "(A)", "  %V   ISO week number, with Monday as first day of week (01..53)"
            print "(A)", "  %w   day of week (0..6); 0 is Sunday"
            print "(A)", "  %W   week number of year, with Monday as first day of week (00..53)"
            print "(A)", "  %x   locale's date representation (e.g., 12/31/99)"
            print "(A)", "  %X   locale's time representation (e.g., 23:13:48)"
            print "(A)", "  %y   last two digits of year (00..99)"
            print "(A)", "  %Y   year"
            print "(A)", "  %z   +hhmm numeric time zone (e.g., -0400)"
            print "(A)", "  %:z  +hh:mm numeric time zone (e.g., -04:00)"
            print "(A)", "  %::z  +hh:mm:ss numeric time zone (e.g., -04:00:00)"
            print "(A)", "  %:::z  numeric time zone with : to necessary precision (e.g., -04, +05:30)"
            print "(A)", "  %Z   alphabetic time zone abbreviation (e.g., EDT)"
            print *
            print "(A)", "By default, date pads numeric fields with zeroes."
            print "(A)", "The following optional flags may follow '%':"
            print *
            print "(A)", "  -  (hyphen) do not pad the field"
            print "(A)", "  _  (underscore) pad with spaces"
            print "(A)", "  0  (zero) pad with zeros"
            print "(A)", "  ^  use upper case if possible"
            print "(A)", "  #  use opposite case if possible"
            print *
            print "(A)", "After any flags comes an optional field width, as a decimal number;"
            print "(A)", "then an optional modifier, which is either"
            print "(A)", "E to use the locale's alternate representations if available, or"
            print "(A)", "O to use the locale's alternate numeric symbols if available."
            print *
            print "(A)", "Examples:"
            print "(A)", "Convert seconds since the epoch (1970-01-01 UTC) to a date"
            print "(A)", "  $ date --date='@2147483647'"
            print *
            print "(A)", "Show the time on the west coast of the US (use tzselect(1) to find TZ)"
            print "(A)", "  $ TZ='America/Los_Angeles' date"
            print *
            print "(A)", "Show the local time for 9AM next Friday on the west coast of the US"
            print "(A)", "  $ date --date='TZ=""America/Los_Angeles"" 09:00 next Fri'"
            print *
            print "(A)", "GNU coreutils online help: <http://www.gnu.org/software/coreutils/>"
            print "(A)", "Report date translation bugs to <http://translationproject.org/team/>"
            print "(A)", "Full documentation at: <http://www.gnu.org/software/coreutils/date>"
            print "(A)", "or available locally via: info '(coreutils) date invocation'"     
        end if

        call exit(status)
    end subroutine usage
    
end program mydate