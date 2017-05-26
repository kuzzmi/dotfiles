#!/bin/bash
account="kuzzmi-gmail"
mail_folder="$HOME/.mail"
new_dir="$mail_folder/$account/INBOX/new"
message_file="$mail_folder/$account.new"
message_tmp_file="$mail_folder/$account.tmp"
num_mail=$(ls $new_dir | wc -l)
num_mail_file="$mail_folder/$account.num"
message=""

if [ ! -f $message_file ]; then
    touch $message_file
fi

echo $num_mail > $num_mail_file

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
            message="$message\nFrom: $(grep -m1 '^From: ' $i|sed 's/From: //'|sed 's/ <[^>]*>//')\n"
        done
        echo $diff
        echo $actual_num_mail
        notify-send "New Email ($actual_num_mail)" "$message" &
    fi
else
    rm $message_file
    rm $num_mail_file
    touch $message_file
fi

if [ -f $message_tmp_file ]; then
    cat $message_tmp_file > $message_file
    rm $message_tmp_file
fi

notmuch new &
