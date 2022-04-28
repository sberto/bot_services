FILE=$(dirname "$0")/../config/sensitive_data.config
read -p 'Bot Name: ' name
read -p 'Bot Token: ' token

if test -f "$FILE"; then
    rm $FILE
fi

echo "{bot_name, \"<<\\\"$name\\\">>\"}." >> $FILE
echo "{bot_token, \"<<\\\"$token\\\">>\"}." >> $FILE
