#!/bin/bash
account="kuzzmi-gmail"
new_dir="$HOME/.mail/$account/INBOX/new"
message_file="$HOME/.mail/$account.new"
message_tmp_file="$HOME/.mail/$account.tmp"
num_mail=$(ls $new_dir | wc -l)
message=""

if [ ! -f $message_file ]; then
    touch $message_file
fi

if [ $num_mail -gt 0 ]; then
    for i in $new_dir/*; do
        echo $i >> $message_tmp_file
    done

    sort $message_tmp_file -o $message_tmp_file

    diff=$(comm -13 $message_file $message_tmp_file)
    actual_num_mail=$(echo -n $diff | wc -w)

    echo $diff
    echo $actual_num_mail

    if [ $actual_num_mail -gt 0 ]; then
        for i in $(echo $diff); do
            message="$message\n$(grep -m1 '^From: ' $i|sed 's/From: //'|sed 's/ <[^>]*>//')\n$(grep -m1 '^Subject: ' $i|sed 's/Subject: //')\n"
        done
        echo $diff
        echo $actual_num_mail
        notify-send "New Email ($actual_num_mail)" "$message" &
    fi
else
    rm $message_file
    touch $message_file
fi

if [ -f $message_tmp_file ]; then
    cat $message_tmp_file > $message_file
    rm $message_tmp_file
fi

notmuch new &
