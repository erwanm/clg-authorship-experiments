if [ -z "$CLG_AUTHORSHIP_PACKAGES_PATH" ]; then
    echo "$0: ERROR, environment variable \$CLG_AUTHORSHIP_PACKAGES_PATH must be set." 1>&2
    exit 1
fi

export PERL5LIB=$PERL5LIB:$ADD_TO_PERL5LIB

source "$CLG_AUTHORSHIP_PACKAGES_PATH"/erw-bash-commons/lib/init-erw-pm.sh
erw-pm addrepo "$CLG_AUTHORSHIP_PACKAGES_PATH"
erw-pm activate clg-authorship-analytics

