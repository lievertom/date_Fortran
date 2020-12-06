program mydate

    implicit none

    character(*), parameter :: rfc_2822_format = "%a, %d %b %Y %H:%M:%S %z"
    character(*), parameter :: utc = "TZ=UTC0 "
    
    logical :: set_date = .false.
    logical :: ok = .false.
    
    integer :: i = 1
    integer :: option_specified_date = 0
    
    integer, dimension(8) :: when
    integer, dimension(9) :: gmt
    
    character(len=32) :: optc = ""
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

    call fdate(date)
    call gmtime(time(), gmt)
    call date_and_time (values=when)

    do
        call get_command_argument(i, optc)
        if (len_trim(optc) == 0) exit
        i = i + 1

        select case (optc)
            case ("-d")
                call getarg(i, datestr)
                i = i + 1
            case ("-f")
                call getarg(i, batch_file)
                i = i + 1
            case ("-I", "--iso-8601")
                new_format = iso_8601_format(1)
            case ("-r")
                call getarg(2, reference)
            case ("-R", "--rfc-2822")
                new_format = rfc_2822_format
            case ("-s")
                call getarg(2, set_datestr)
                set_date = .true.
            case ("-u", "--utc", "--univesrsal")
                if (when(4) /= 0) then
                    call get_command(optc)
                    call execute_command_line(utc//optc)
                    call exit()
                end if
            case ("--help")
                call usage(.true.)
            case ("--version")
                print *, "Beta"
                print *, "Escrito por Lieverton"
                call exit()
            case default
                if (optc(1:11) == "--rfc-3339=") then
                    new_format = rfc_3339_format(rfc_3339_fmt(trim(optc(12:))))
                else if (optc(1:2) == "-I") then
                    new_format = iso_8601_format(iso_8601_fmt(trim(optc(3:))))
                else if (optc(1:11) == "--iso-8601=") then
                    new_format = iso_8601_format(iso_8601_fmt(trim(optc(12:))))
                else if (optc(1:1) == "+") then
                    new_format = optc(2:)
                else
                    call usage(.false.)
                end if
            end select
            
            if (new_format /= "") then
                if (my_format /= "") then
                    write (*, *) "multiple output formats specified"
                end if
                my_format = new_format
            end if
        end do
        
        option_specified_date = merge(1, 0, datestr /= "") + merge(1, 0, batch_file /= "") + merge(1, 0, reference /= "")
        
        if (option_specified_date > 1) then
            call usage(.false.)
            write (*, *) "the options to specify dates for printing are mutually exclusive"
        end if
        
        if (set_date .and. option_specified_date > 0) then
            write (*, *) "the options to print and set the time may not be used together"
            call usage(.false.)
        end if
        
        if (my_format == "") then
            my_format = "%a %b %e %H:%M:%S %Z %Y"
        end if
        
        if (batch_file /= "") then
        print *, 'file'
    else
        ok = .true.
        if (option_specified_date == 0 .and. .not.set_date) then
            
        end if
        ok = ok .and. show_date (my_format, when, date, gmt)
    end if

contains

    function rfc_3339_fmt(arg)
        implicit none
        character (*), intent(in) :: arg
        integer :: rfc_3339_fmt

        select case (arg)
            case ("date")
                rfc_3339_fmt = 1
            case ("seconds")
                rfc_3339_fmt = 2
            case ("ns")
                rfc_3339_fmt = 3
            case default
                print *, 'date: “'//arg//'” is an invalid argument for “--rfc-3339”'
                print *, 'valid arguments are:'
                print *, '  - “date”'
                print *, '  - “seconds”'
                print *, '  - “ns”'
                call usage(.false.)
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
            case ("ns")
                iso_8601_fmt = 5
            case default
                print *, 'date: “'//arg//'” is an invalid argument for “--iso-8061”'
                print *, 'valid arguments are:'
                print *, '  - “hours”'
                print *, '  - “minutes”'
                print *, '  - “date”'
                print *, '  - “seconds”'
                print *, '  - “ns”'
                call usage(.false.)
        end select

    end function iso_8601_fmt
 
    function show_date (my_format, when, date, gmt)
        implicit none
        character(len=32), intent(in) :: my_format, date
        integer, dimension(8), intent(in) :: when
        integer, dimension(9), intent(in) :: gmt
        character(len=4) :: c
        logical :: show_date
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
                call parser_format(c, when, date, gmt)
            else
                write (*,"(a1)",advance="no") c
            end if
        end do
        print *,
        show_date = .true.
    end function show_date

    subroutine parser_format (c, when, date, gmt)
        implicit none
        character(*), intent(in) :: c
        character(len=32), intent(in) :: date
        integer, dimension(8), intent(in) :: when
        integer, dimension(9), intent(in) :: gmt

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
                write (*,"(i2.2)",advance="no") when(1)/100 + merge(1,0,mod(when(1),100)>0)
            case ("d")
                write (*,"(i2.2)",advance="no") when(3)
            case ("D")
                write (*,"(i2.2,a1,i2.2,a1,a2)",advance="no") when(3), "/", when(2), "/", date(23:24)
            case ("e")
                write (*,"(a2)",advance="no") date(9:10)
            case ("F")
                write (*,"(i4.4,a1,i2.2,a1,i2.2)",advance="no") when(1), "-", when(2), "-", when(3)
            case ("g")
                write (*,"(a2)",advance="no") date(23:24)
            case ("G")
                write (*,"(a4)",advance="no") date(21:24)
            case ("h")
                write (*,"(a3)",advance="no") date(5:7)
            case ("H")
                write (*,"(a2)",advance="no") date(12:13)
            case ("I")
                write (*,"(i2.2)",advance="no") mod(when(5),12) + merge(12,0,mod(when(5),12)==0)
            case ("j")
                write (*,"(i3.3)",advance="no") gmt(8) + 1
            case ("k")
                write (*,"(i2)",advance="no") when(5)
            case ("l")
                write (*,"(i2)",advance="no") mod(when(5),12) + merge(12,0,mod(when(5),12)==0)
            case ("m")
                write (*,"(i2.2)",advance="no") when(2)
            case ("M")
                write (*,"(a2)",advance="no") date(15:16)
            case ("n")
                print *,
            case ("N")
                write (*,"(i3.3,a6)",advance="no") when(8), "000000"
            case ("p")
                ! 
            case ("P")
                !
            case ("r")
                write (*,"(i2.2)",advance="no") mod(when(5),12) + merge(12,0,mod(when(5),12)==0)
                write (*,"(a1,i2.2,a1,i2.2)",advance="no") ":", when(6), ":", when(7)
            case ("R")
                write (*,"(a5)",advance="no") date(12:16)
            case ("s")
                ! 
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
                write (*,"(i2.2,a1,i2.2,a1,a2)",advance="no") when(3), "/", when(2), "/", date(23:24)
            case ("X")
                write (*,"(a8)",advance="no") date(12:19)
            case ("y")
                write (*,"(a8)",advance="no") date(23:24)
            case ("Y")
                write (*,"(a4)",advance="no") date(21:24)
            case ("z")
                if (when(4) >= 0) then
                    write (*,"(a1,i2.2,i2.2)",advance="no") "+", when(4)/60, mod(when(4), 60)
                else
                    write (*,"(i3.2,i2.2)",advance="no") when(4)/60, mod(when(4), 60)
                end if
            case (":z")
                if (when(4) >= 0) then
                    write (*,"(a1,i2.2,a1,i2.2)",advance="no") "+", when(4)/60, ":", mod(when(4), 60)
                else
                    write (*,"(i3.2,a1,i2.2)",advance="no") when(4)/60, ":", mod(when(4), 60)
                end if
            case ("::z")
                if (when(4) >= 0) then
                    write (*,"(a1,i2.2,a1,i2.2,a3)",advance="no") "+", when(4)/60, ":", mod(when(4), 60), ":00"
                else
                    write (*,"(i3.2,a1,i2.2,a3)",advance="no") when(4)/60, ":", mod(when(4), 60), ":00"
                end if
            case (":::z")
                if (when(4) >= 0) then
                    write (*,"(a1,i2.2)",advance="no") "+", when(4)/60
                else
                    write (*,"(i3.2)",advance="no") when(4)/60
                end if
                if (mod(when(4), 60) > 0) then
                    write (*,"(a1,i2.2)",advance="no") ":", modulo(when(4), 60)
                end if
            case ("Z")
                if (when(4) == 0) then
                    write (*,"(a3)",advance="no") "UTC"
                else
                    write (*,"(i3.2)",advance="no") when(4)/60
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

    subroutine usage (status)
        implicit none
        logical, intent(in) :: status

        if (status) then
            write (*, *) "Uso: mydate [OPÇÃO]... [+FORMATO]"
            write (*, *) " ou: mydate [-u|--utc|--universal] [MMDDhhmm[[CC]YY][.ss]]"
            write (*, *) "Display the current time in the given FORMAT, or set the system date."
            write (*, *) ""
            write (*, *) "Mandatory arguments to long options are mandatory for short options too."
            write (*, *) "  -d, --date=STRING         display time described by STRING, not 'now'"
            write (*, *) "  -f, --file=DATEFILE        like --date; once for each line of DATEFILE"
            write (*, *) "  -I[FMT], --iso-8601[=FMT]  output date/time in ISO 8601 format."
            write (*, *) "                               FMT='date' for date only (the default),"
            write (*, *) "                               'hours', 'minutes', 'seconds', or 'ns'"
            write (*, *) "                               for date and time to the indicated precision."
            write (*, *) "                               Example: 2006-08-14T02:34:56-0600"
            write (*, *) "  -R, --rfc-2822             output date and time in RFC 2822 format."
            write (*, *) "                               Example: Mon, 14 Aug 2006 02:34:56 -0600"
            write (*, *) "      --rfc-3339=FMT         output date/time in RFC 3339 format."
            write (*, *) "                               FMT='date', 'seconds', or 'ns'"
            write (*, *) "                               for date and time to the indicated precision."
            write (*, *) "                               Example: 2006-08-14 02:34:56-06:00"
            write (*, *) "  -r, --reference=FILE       display the last modification time of FILE"
            write (*, *) "  -s, --set=STRING           set time described by STRING"
            write (*, *) "  -u, --utc, --universal     print or set Coordinated Universal Time (UTC)"
            write (*, *) "      --help     mostra esta ajuda e finaliza"
            write (*, *) "      --version  informa a versão e finaliza"
            write (*, *) ""
            write (*, *) "FORMATO controla a saída. As sequências interpretadas são:"
            write (*, *) ""
            write (*, *) "  %%   um % literal"
            write (*, *) "  %a   nome abreviado do dia de semana da localidade (por exemplo, Sáb)"
            write (*, *) "  %A   nome completo do dia de semana na localidade (por exemplo, Sábado)"
            write (*, *) "  %b   nome abreviado do mês na localidade (por exemplo, Jan)"
            write (*, *) "  %B   nome completo do mês na localidade (por exemplo, Janeiro)"
            write (*, *) "  %c   data e hora na localidade (por exemplo, Sáb 08 Mar 2008 18:34:17 BRT)"
            write (*, *) "  %C   século; como %Y, mas omite os dois últimos dígitos (por exemplo, 21)"
            write (*, *) "  %d   dia do mês (por exemplo, 01)"
            write (*, *) "  %D   data no formato estado-unidense; o mesmo que %d/%m/%y"
            write (*, *) "  %e   dia do mês, preenchido com espaço; o mesmo que %_d"
            write (*, *) "  %F   data completa; o mesmo que %Y-%m-%d"
            write (*, *) "  %g   os últimos dois dígitos do ano do número ISO da semana (veja %G)"
            write (*, *) "  %G   ano do número ISO da semana ISO (veja %V); normalmente útil só com %V"
            write (*, *) "  %h   o mesmo que %b"
            write (*, *) "  %H   hora (00..23)"
            write (*, *) "  %I   hora (01..12)"
            write (*, *) "  %j   dia do ano (001..366)"
            write (*, *) "  %k   hora, com preenchimento de espaço ( 0..23); o mesmo que %_H"
            write (*, *) "  %l   hora, com preenchimento de espaço ( 1..12); o mesmo que %_I"
            write (*, *) "  %m   mês (01..12)"
            write (*, *) "  %M   minuto (00..59)"
            write (*, *) "  %n   um caractere de nova-linha"
            write (*, *) "  %N   nanosegundos (000000000..999999999)"
            write (*, *) "  %p   o equivalente na localidade para AM ou PM; em branco se desconhecido"
            write (*, *) "  %P   como %p, mas em minúsculas"
            write (*, *) "  %r   a hora no relógio de 12 horas na localidade (por exemplo, 11:11:04 PM)"
            write (*, *) "  %R   hora e minuto no estilo 24 horas; o mesmo que %H:%M"
            write (*, *) "  %s   segundos desde 1970-01-01 00:00:00 UTC"
            write (*, *) "  %S   segundo (00..60)"
            write (*, *) "  %t   uma tabulação"
            write (*, *) "  %T   as horas; o mesmo que %H:%M:%S"
            write (*, *) "  %u   dia da semana (1..7); 1 é segunda-feira"
            write (*, *) "  %U   número da semana no ano, sendo domingo o início da semana (00..53)"
            write (*, *) "  %V   número ISO da semana, sendo segunda-feira o início da semana (01..53)"
            write (*, *) "  %w   dia da semana (0..6); 0 é domingo"
            write (*, *) "  %W   número da semana no ano, sendo segunda-feira o início da semana (00..53)"
            write (*, *) "  %x   representação da data na localidade (por exemplo, 31/12/99)"
            write (*, *) "  %X   representação da hora na localidade (por exemplo, 23:13:48)"
            write (*, *) "  %y   os últimos dois dígitos do ano (00..99)"
            write (*, *) "  %Y   ano"
            write (*, *) "  %z   fuso horário numérico +hhmm (por exemplo, -0400)"
            write (*, *) "  %:z  fuso horário numérico +hh:mm (por exemplo, -04:00)"
            write (*, *) "  %::z  fuso horário numérico +hh:mm:ss (por exemplo, -04:00:00)"
            write (*, *) "  %:::z  fuso horário numérico com : para a precisão necessária"
            write (*, *) "           (por exemplo, -04, +05:30)"
            write (*, *) "  %Z   abreviação alfabética do fuso horário (por exemplo, BRT)"
            write (*, *) ""
            write (*, *) "Por padrão, campos numéricos de data são preenchidos com zeros."
            write (*, *) "As seguintes opções sinalizadoras podem seguir '%':"
            write (*, *) ""
            write (*, *) "  -  (hífen) não preencher o campo"
            write (*, *) "  _  (sublinhado) preencher com espaços"
            write (*, *) "  0  (zero) preencher com zeros"
            write (*, *) "  ^  usar letras maiúsculas, se possível"
            write (*, *) "  #  usar capitalização oposta, se possível"
            write (*, *) ""
            write (*, *) "Depois de qualquer sinalizador pode haver uma largura de campo opcional,"
            write (*, *) "como um número decimal; então, um modificador também opcional, que pode ser"
            write (*, *) " E para usar as representações alternativas da localidade, se disponível, ou"
            write (*, *) " O para usar os símbolos númericos alternativos da localidade, se disponível."
            write (*, *) ""
            write (*, *) "Exemplos:"
            write (*, *) "Converte segundos desde o período (1970-01-01 UTC) até uma data"
            write (*, *) "  $ mydate --date='@2147483647'"
            write (*, *) ""
            write (*, *) "Mostra a hora na costa oeste dos EUA (use tzselect(1) para encontrar TZ)"
            write (*, *) "  $ TZ='America/Los_Angeles' mydate"
            write (*, *) ""
            write (*, *) "Mostra a hora local para 9AM próxima sexta-feira na costa oeste dos EUA"
            write (*, *) "  $ mydate --date='TZ='America/Los_Angeles' 09:00 next Fri'"
            write (*, *) ""          
        else
            write (*, *) "Try './mydate --help' for more information."
        end if
        call exit()
    end subroutine usage
    
    
end program mydate