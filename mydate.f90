program mydate

    implicit none

    character(len=24), parameter :: rfc_2822_format = "%a, %d %b %Y %H:%M:%S %z"

    logical :: set_date = .false.
    ! logical :: valid_date = .false.
    logical :: ok = .false.

    integer :: i = 1
    integer :: option_specified_date = 0
    
    integer, dimension(8) :: when

    character(len=32) :: optc = ""
    character(len=32) :: datestr = "" 
    character(len=32) :: batch_file = ""
    character(len=32) :: reference = ""
    character(len=32) :: new_format = ""
    character(len=32) :: my_format = ""
    character(len=32) :: set_datestr = ""
    character(len=32) :: error = ""
    character(len=32) :: date = ""

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
            ! case ("--rfc-3339")
            !     call getarg(2, batch_file)
            ! case ("-I")
            !     call getarg(2, batch_file)
            case ("-r")
                call getarg(2, reference)
            case ("-R")
                new_format = rfc_2822_format
            case ("-s")
                call getarg(2, set_datestr)
                set_date = .true.
            ! case ("-u")
            !     call getarg(2, set_datestr)
            !     set_date = .true.
            case default
                call usage(.false.)
        end select

        if (new_format /= "") then
            if (my_format /= "") then
                write (*, *) "multiple output formats specified"
            end if
            my_format = new_format
        end if
    END DO    

    option_specified_date = merge(1, 0, datestr /= "") + merge(1, 0, batch_file /= "") + merge(1, 0, reference /= "")

    if (option_specified_date > 1) then
        call usage(.false.)
        write (*, *) "the options to specify dates for printing are mutually exclusive"
    end if

    if (set_date .and. option_specified_date > 0) then
        write (*, *) "the options to print and set the time may not be used together"
        call usage(.false.)
    end if
    
    if (i < iargc()) then
        if (i + 1 < iargc()) then
            call getarg(i+1, error)
            write (*, *) "extra operand ", error
            call usage (.false.)
        end if
        call getarg(i, error)
        if (index(error, '+') > 0) then
            write (*, *) "multiple output formats specified"
            my_format = error
            i = i + 1
        else if (set_date .or. option_specified_date > 0) then
            call getarg(i, error)
            write (*, *) "the argument "//error//" lacks a leading '+'"
            write (*, *) "when using an option to specify date(s), any non-option"
            write (*, *)  "argument must be a format string beginning with '+'"
            call usage (.false.)
        end if
    end if

    if (my_format == "") then
        my_format = "%a %b %e %H:%M:%S %Z %Y"
    end if
    
    if (batch_file /= "") then
        print *, 'file'
    else
        ok = .true.
        if (option_specified_date == 0 .and. .not.set_date) then
            if (i < iargc()) then
                set_date = .true.
                call getarg(iargc(), datestr)
                ! valid_date = 
            else
                date = fdate()
                call date_and_time (values=when)
                when(4) = when(4) / 60
            end if
        end if
        ok = ok .and. show_date (my_format, when, date)
    end if


contains

    function show_date (my_format, when, date)
        character(len=32) :: my_format, date
        integer, dimension(8) :: when
        logical :: show_date
        if (my_format == "%a %b %e %H:%M:%S %Z %Y") then
            print "(a20,i3.2,1x,a12)", date(1:20), when(4), date(21:)
            show_date = .true.
        end if
    end function show_date

    subroutine usage (status)
        logical :: status

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
            write (*, *) "Try 'mydate --help' for more information."
        end if
        
    end subroutine usage
    
    
end program mydate