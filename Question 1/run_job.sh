#!/bin/bash
# run_job.sh - Submit the keyword search as a Hadoop Streaming job
# Usage: ./run_job.sh <keyword>

KEYWORD="$1"
INPUT=/user/mikay/map_reduce_test_docs
OUTPUT=/user/mikay/keyword_output

hdfs dfs -rm -r -f $OUTPUT

hadoop jar $HADOOP_HOME/share/hadoop/tools/lib/hadoop-streaming-*.jar \
-D mapreduce.job.name="Keyword search - $KEYWORD" \
-D mapreduce.job.reduces=1 \
-D mapreduce.input.fileinputformat.split.minsize=1073741824 \
-D stream.num.map.output.key.fields=1 \
-files mapper.R,reducer.R \
-mapper "Rscript mapper.R $KEYWORD" \
-reducer "Rscript reducer.R" \
-input $INPUT \
-output $OUTPUT