@echo off

REM Scopes the variables to the current batch file
REM setlocal

rem Preserve values that might contain exclamation marks before
rem enabling delayed expansion
set TDP0=%~dp0
REM setlocal enabledelayedexpansion

REM SCRIPT_DIR=`dirname $SCRIPT_PATH`
REM RABBITMQ_HOME="${SCRIPT_DIR}/.."
set SCRIPT_DIR=%TDP0%
set SCRIPT_NAME=%1
for /f "delims=" %%F in ("%SCRIPT_DIR%..") do set RABBITMQ_HOME=%%~dpF%%~nF%%~xF

REM If ERLANG_HOME is not defined, check if "erl.exe" is available in
REM the path and use that.
if not defined ERLANG_HOME (
    for /f "delims=" %%F in ('where.exe erl.exe') do @set ERL_PATH=%%F
    if exist "!ERL_PATH!" (
        for /f "delims=" %%F in ("!ERL_PATH!") do set ERL_DIRNAME=%%~dpF
        for /f "delims=" %%F in ("!ERL_DIRNAME!\..") do @set ERLANG_HOME=%%~dpF%%~nF%%~xF
    )
    set ERL_PATH=
    set ERL_DIRNAME=
)

REM ## Set defaults
call "%SCRIPT_DIR%\rabbitmq-defaults.bat"

if "!RABBITMQ_CONF_ENV_FILE!"=="" (
    set RABBITMQ_CONF_ENV_FILE=!CONF_ENV_FILE:"=!
) else (
    set RABBITMQ_CONF_ENV_FILE=!RABBITMQ_CONF_ENV_FILE:"=!
)

if exist "!RABBITMQ_CONF_ENV_FILE!" (
    call "!RABBITMQ_CONF_ENV_FILE!"
)

set DEFAULT_SCHEDULER_BIND_TYPE=db
if "!RABBITMQ_SCHEDULER_BIND_TYPE!"=="" (
    set RABBITMQ_SCHEDULER_BIND_TYPE=!SCHEDULER_BIND_TYPE!
)
if "!RABBITMQ_SCHEDULER_BIND_TYPE!"=="" (
    set RABBITMQ_SCHEDULER_BIND_TYPE=!DEFAULT_SCHEDULER_BIND_TYPE!
)

set DEFAULT_DISTRIBUTION_BUFFER_SIZE=128000
if "!RABBITMQ_DISTRIBUTION_BUFFER_SIZE!"=="" (
    set RABBITMQ_DISTRIBUTION_BUFFER_SIZE=!DISTRIBUTION_BUFFER_SIZE!
)
if "!RABBITMQ_DISTRIBUTION_BUFFER_SIZE!"=="" (
    set RABBITMQ_DISTRIBUTION_BUFFER_SIZE=!DEFAULT_DISTRIBUTION_BUFFER_SIZE!
)

set DEFAULT_MAX_NUMBER_OF_PROCESSES=1048576
if "!RABBITMQ_MAX_NUMBER_OF_PROCESSES!"=="" (
    set RABBITMQ_MAX_NUMBER_OF_PROCESSES=!MAX_NUMBER_OF_PROCESSES!
)
if "!RABBITMQ_MAX_NUMBER_OF_PROCESSES!"=="" (
    set RABBITMQ_MAX_NUMBER_OF_PROCESSES=!DEFAULT_MAX_NUMBER_OF_PROCESSES!
)

set DEFAULT_MAX_NUMBER_OF_ATOMS=5000000
if "!RABBITMQ_MAX_NUMBER_OF_ATOMS!"=="" (
    set RABBITMQ_MAX_NUMBER_OF_ATOMS=!MAX_NUMBER_OF_ATOMS!
)
if "!RABBITMQ_MAX_NUMBER_OF_ATOMS!"=="" (
    set RABBITMQ_MAX_NUMBER_OF_ATOMS=!DEFAULT_MAX_NUMBER_OF_ATOMS!
)

REM Common server defaults
set SERVER_ERL_ARGS=+P !RABBITMQ_MAX_NUMBER_OF_PROCESSES! +t !RABBITMQ_MAX_NUMBER_OF_ATOMS! +stbt !RABBITMQ_SCHEDULER_BIND_TYPE! +zdbbl !RABBITMQ_DISTRIBUTION_BUFFER_SIZE!

REM ##--- Set environment vars RABBITMQ_<var_name> to defaults if not set

REM [ "x" = "x$RABBITMQ_SERVER_ERL_ARGS" ] && RABBITMQ_SERVER_ERL_ARGS=${SERVER_ERL_ARGS}
if "!RABBITMQ_SERVER_ERL_ARGS!"=="" (
    set RABBITMQ_SERVER_ERL_ARGS=!SERVER_ERL_ARGS!
)

REM [ "x" = "x$RABBITMQ_SERVER_START_ARGS" ] && RABBITMQ_SERVER_START_ARGS=${SERVER_START_ARGS}
if "!RABBITMQ_SERVER_START_ARGS!"=="" (
    if not "!SERVER_START_ARGS!"=="" (
        set RABBITMQ_SERVER_START_ARGS=!SERVER_START_ARGS!
    )
)

REM [ "x" = "x$RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS" ] && RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS=${SERVER_ADDITIONAL_ERL_ARGS}
if "!RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS!"=="" (
    if not "!SERVER_ADDITIONAL_ERL_ARGS!"=="" (
        set RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS=!SERVER_ADDITIONAL_ERL_ARGS!
    )
)

REM [ "x" = "x$RABBITMQ_BOOT_MODULE" ] && RABBITMQ_BOOT_MODULE=${BOOT_MODULE}
if "!RABBITMQ_BOOT_MODULE!"=="" (
    if "!BOOT_MODULE!"=="" (
        set RABBITMQ_BOOT_MODULE=rabbit
    ) else (
        set RABBITMQ_BOOT_MODULE=!BOOT_MODULE!
    )
)

REM [ "x" = "x$RABBITMQ_CTL_ERL_ARGS" ] && RABBITMQ_CTL_ERL_ARGS=${CTL_ERL_ARGS}
if "!RABBITMQ_CTL_ERL_ARGS!"=="" (
    if not "!CTL_ERL_ARGS!"=="" (
        set RABBITMQ_CTL_ERL_ARGS=!CTL_ERL_ARGS!
    )
)

REM ADDITIONAL WINDOWS ONLY CONFIG ITEMS

if "!RABBITMQ_SERVICENAME!"=="" (
    if "!SERVICENAME!"=="" (
        set RABBITMQ_SERVICENAME=RabbitMQ
    ) else (
        set RABBITMQ_SERVICENAME=!SERVICENAME!
    )
)

REM Environment cleanup
set BOOT_MODULE=
set CONFIG_FILE=
set FEATURE_FLAGS_FILE=
set ENABLED_PLUGINS_FILE=
set LOG_BASE=
set MNESIA_BASE=
set PLUGINS_DIR=
set SCRIPT_DIR=
set SCRIPT_NAME=
set TDP0=

REM ##--- End of overridden <var_name> variables

REM # Since we source this elsewhere, don't accidentally stop execution
REM true
