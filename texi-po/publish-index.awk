#
# gawk -f publish-index.awk TEMPLATE=<<template_path>> OUTPUT=<<output_file_path>> < ~/work/git-docs-ja/GIT-VERSION-FILE 

#
/GIT_VERSION/ {
    split($_, item, "=")
    # print item[2]
    next
}

END {
    # print "GIT_VERSION=" "'" item[2] "' envsubst < " TEMPLATE " > " OUTPUT
    system("GIT_VERSION=" "'" item[2] "' envsubst < " TEMPLATE " > " OUTPUT)
}

