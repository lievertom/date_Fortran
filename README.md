# Date em Fortran

## Proposta

Este trabalho tem por objetivo a realização do porte da ferramenta [date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation) do [coreutils](https://www.gnu.org/software/coreutils/) do *Linux*.

## Como Rodar

### Instalação do GNU Fortran

O projeto GNU Fortran [GFortran](https://gcc.gnu.org/fortran/) consiste em um *front-end* de
compilador e bibliotecas de *run-time* para o GCC que fornecem suporte `a linguagem Fortran.

Em distribuições Linux com suporte ao apt, ele pode ser instalado com o comando:

```shell
sudo apt-get update && sudo apt-get install gfortran
```

### Execução do Projeto

1. Clone o repositório:

    ```shell
    git clone https://github.com/lievertom/date_Fortran.git
    ```

2. Entre no diretório do projeto:

    ```bash
    cd date_Fortran/
    ```

3. Compile:

    ```bash
    make
    ```

4. Execute:

    ```bash
    ./mydate
    ```

#### Comandos

```shell
./mydate [option]… [+format]
./mydate [-u|--utc|--universal] [ MMDDhhmm[[CC]YY][.ss] ]
```

- Para mais informações rode:

```shell
./mydate --help
```

## Autor

|Matrícula | Estudante |
| -- | -- |
| 17/0039251 | Lieverton Santos Silva |

## Referências

- [Código Original](https://github.com/wertarbyte/coreutils/blob/master/src/date.c)

- [Manual Coreutils](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)

- [Documentação Gfortran](https://gcc.gnu.org/onlinedocs/gfortran/)

- [Slides do Curso de Paradigmas](https://github.com/edsomjr/Paradigmas/tree/master/Programacao_Estruturada/slides)
