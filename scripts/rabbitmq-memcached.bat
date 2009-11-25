@echo off
REM   The contents of this file are subject to the Mozilla Public License
REM   Version 1.1 (the "License"); you may not use this file except in
REM   compliance with the License. You may obtain a copy of the License at
REM   http://www.mozilla.org/MPL/
REM
REM   Software distributed under the License is distributed on an "AS IS"
REM   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
REM   License for the specific language governing rights and limitations
REM   under the License.
REM

setlocal

if not exist "%ERLANG_HOME%\bin\erl.exe" (
    echo.
    echo ******************************
    echo ERLANG_HOME not set correctly. 
    echo ******************************
    echo.
    echo Please either set ERLANG_HOME to point to your Erlang installation or place the
    echo RabbitMQ server distribution in the Erlang lib folder.
    echo.
    exit /B
)

set RABBITMQ_MEMCACHED_EBIN_ROOT=%~dp0..\ebin
set RABBITMQ_ERLANG_CLIENT_ROOT=%~dp0..\..\rabbitmq-erlang-client
set RABBITMQ_ERLANG_CLIENT_EBIN_ROOT=%RABBITMQ_ERLANG_CLIENT_ROOT%\ebin
set RABBITMQ_COMMON_EBIN_ROOT=%RABBITMQ_ERLANG_CLIENT_ROOT%\deps\rabbit_common\ebin

set RABBITMQ_MEMCACHED_EBIN_PATH=-pz "%RABBITMQ_MEMCACHED_EBIN_ROOT%" "%RABBITMQ_ERLANG_CLIENT_EBIN_ROOT%" "%RABBITMQ_COMMON_EBIN_ROOT%"

"%ERLANG_HOME%\bin\erl.exe" ^
%RABBITMQ_MEMCACHED_EBIN_PATH% ^
-s application start rabbit_memcached ^
-boot start_sasl

endlocal