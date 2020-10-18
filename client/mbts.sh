#!/bin/sh

set -e

pidfile=~/.mbts/pid

if [ $# -gt 0 ]; then
	if [ $1 == "exit" ]; then
		pid=`cat $pidfile`
		if [ $pid != "" ]; then
			kill -s 15 -$pid
			exit
		fi
	fi
fi
