#!/bin/bash

ERLANG_SCRIPT="/work/helios/server/bin/server"
CDN_URL="http://cdn.yourcompany.com/"
WWW_ROOT="/work/www/cdn.yourcompany.com"
TMP_DIR="/tmp/ci"
SEPARATOR="####"

is_pipeline_ok () {
    read -r -d '' args <<< "$@"

    for arg in $args; do
        if [ "$arg" != "ok" ] ; then
            break -1
        fi
    done
    return $?
}

if [ "$1" = "start" ]; then
    CMD="$ERLANG_SCRIPT start"

elif [ "$1" = "stop" ]; then
    CMD="$ERLANG_SCRIPT stop"

elif [ "$1" = "restart" ]; then
    CMD="$ERLANG_SCRIPT restart"

elif [ "$1" = "create-game" ]; then
    # $2 :: game GUID
    # $3 :: game title
    CMD="$ERLANG_SCRIPT rpc server_rpc create_game \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "rename-game" ]; then
    # $2 :: game GUID
    # $3 :: game title
    CMD="$ERLANG_SCRIPT rpc server_rpc rename_game \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "publish-game" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc publish_game \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "unpublish-game" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc unpublish_game \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "enable-game" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc enable_game \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "disable-game" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc disable_game \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "set-jira-key" ]; then
    # $2 :: game GUID
    # $3 :: JIRA key
    CMD="$ERLANG_SCRIPT rpc server_rpc set_jira_key \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "unset-jira-key" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc unset_jira_key \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "set-selene-key" ]; then
    # $2 :: game GUID
    # $3 :: Selene key
    CMD="$ERLANG_SCRIPT rpc server_rpc set_selene_key \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "unset-selene-key" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc unset_selene_key \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "set-discord-url" ]; then
    # $2 :: game GUID
    # $3 :: Discord URL
    CMD="$ERLANG_SCRIPT rpc server_rpc set_discord_url \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "unset-discord-url" ]; then
    # $2 :: game GUID
    CMD="$ERLANG_SCRIPT rpc server_rpc unset_discord_url \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "create-branch" ]; then
    # $2 :: game GUID
    # $3 :: branch title
    # $4 :: branch password
    CMD="$ERLANG_SCRIPT rpc server_rpc create_branch \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\" \\\"${4// /$SEPARATOR}\\\""

elif [ "$1" = "set-default-branch" ]; then
    # $2 :: game GUID
    # $3 :: branch title
    CMD="$ERLANG_SCRIPT rpc server_rpc set_default_branch \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "set-public-branch" ]; then
    # $2 :: game GUID
    # $3 :: branch title
    CMD="$ERLANG_SCRIPT rpc server_rpc set_public_branch \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "unset-public-branch" ]; then
    # $2 :: game GUID
    # $3 :: branch title
    CMD="$ERLANG_SCRIPT rpc server_rpc unset_public_branch \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\""

elif [ "$1" = "set-build-changelog" ]; then
    # $2 :: game GUID
    # $3 :: build revision
    # $4 :: path to changelog file
    CMD="$ERLANG_SCRIPT rpc server_rpc set_build_changelog \\\"${2// /$SEPARATOR}\\\" \\\"${3// /$SEPARATOR}\\\" \\\"${4// /$SEPARATOR}\\\""

elif [ "$1" = "prepare-build" ]; then
    # $2 :: input directory
    # $3 :: game GUID
    # $4 :: branch
    # $5 :: build-rev
    # $6 :: CDN root URL prefix
    # $7 :: output directory
    CMD="helios-prepare.py $2 $3 $4 $5 $6 $7"

elif [ "$1" = "sync-build" ]; then
    # $2 :: input directory (should include game GUID and build-rev like: /tmp/ci/sfl-112 having subdirectory /sfl/112)
    # $3 :: output directory
    CMD="helios-sync.sh $2 $3"

elif [ "$1" = "push-build" ]; then
    # $2 :: path to prepared manifest
    CMD="$ERLANG_SCRIPT rpc server_rpc push_build \\\"${2// /$SEPARATOR}\\\""

elif [ "$1" = "push-local-build" ]; then
    # $2 :: input directory
    # $3 :: game GUID
    # $4 :: branch
    # $5 :: build-rev
    MANIFEST_PATH="$TMP_DIR/$3-$5-manifest.json"
    CMD_PREPARE="helios-prepare.py $2 $3 $4 $5 $CDN_URL $MANIFEST_PATH"
    CMD_MKDIR="mkdir -p $WWW_ROOT/$3/$5"
    CMD_SYNC="helios-sync.sh $2 $WWW_ROOT/$3/$5"
    CMD_PUSH=$"${ERLANG_SCRIPT} rpc server_rpc push_build \\\"${MANIFEST_PATH}\\\""
    CMD_CLEAN="rm -fR $2 && rm $MANIFEST_PATH"
    CMD_ALL=$"${CMD_PREPARE} && ${CMD_MKDIR} && ${CMD_SYNC} && ${CMD_PUSH} && ${CMD_CLEAN}"
    echo $CMD_ALL
    INT_RESULT=`eval $CMD_ALL`
    is_pipeline_ok "${INT_RESULT[@]}"
    [[ "$?" = "0" ]] && INT_RESULT="ok" || INT_RESULT="failure"
    CMD="echo ${INT_RESULT}"

fi

RESULT=`eval $CMD`
[[ $RESULT = "ok" ]] && EXIT_CODE=0 || EXIT_CODE=1
echo "Result: ${RESULT}"
exit $EXIT_CODE
