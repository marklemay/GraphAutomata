#!/bin/bash
set -x

# input_dir=$1
# cd $input_dir
output_dir="/home/wajih/dataset/ftp"

mkdir -p $output_dir

neo_dir="/home/wajih/neo4j-community-3.0.3/data/databases/graph.db"
graph_auto_dir="/home/wajih/GraphAutomata/"
spade_dir="/home/wajih/SPADE/"
stat_dir="/tmp/time_stat.csv"
rm $stat_dir

prev_size=0
prev_time=0

for i in 50 100 150 200 250 300 350;
do
    cd $spade_dir
    ./run-spade.sh
    cd -
    sleep 5
    ftpbench -h 127.0.0.1 -u wajih -p motoracer upload workdir/ -c 1 -s 1  --maxrun=10 &
    sleep $i
    killall -09 ftpbench
    /home/wajih/SPADE/bin/spade stop
    sleep 10
    rm -rf $neo_dir/*
    cp -R /tmp/spade.graph_db/* $neo_dir/
    /home/wajih/neo4j-community-3.0.3/bin/neo4j stop
    sleep 5
    /home/wajih/neo4j-community-3.0.3/bin/neo4j start
    sleep 10
    cd $graph_auto_dir
    mvn test -Dtest=TestNeoGraphs#learnNeo4j
    i_now_size=`cat stats.csv | grep 'i-size-bytes' | cut -d, -f2`
    f_now_size=`cat stats.csv | grep 'f-size-bytes' | cut -d, -f2`
    now_time=`cat stats.csv | grep 'total-time' | cut -d, -f2`
    inc_time=$(echo "$now_time  - $prev_time" | bc)
    prev_time=$now_time
    echo  "$i,$i_now_size,$inc_time,$f_now_size," >> $stat_dir
    cd -
done

exit

for i in `ls $input_dir`; do
    neo4j stop
    sleep 10
    data_set_dir=$input_dir/$i/


    rm -rf $neo_dir/*
    cp -R $data_set_dir/* $neo_dir/
    neo4j start
    sleep 20
    cd $graph_auto_dir
    mvn test -Dtest=TestNeoGraphs#learnNeo4j
    mkdir -p $output_dir/$i/
    cp stats.csv $output_dir/$i/
    cp time_cost.csv $output_dir/$i/
    cd -
done
