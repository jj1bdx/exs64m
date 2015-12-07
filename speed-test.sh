#!/bin/sh
rebar clean
rebar compile
erl -pa ebin -noshell -s exs64m_speed test_speed -s init stop
erl -pa ebin -noshell -s exs64m_speed test_speed -s init stop
erl -pa ebin -noshell -s exs64m_speed test_speed -s init stop
erl -pa ebin -noshell -s exs64m_speed test_speed -s init stop
erl -pa ebin -noshell -s exs64m_speed test_speed -s init stop
