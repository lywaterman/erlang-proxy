#! /bin/bash

# +K true: enable kernel poll
# -boot start_sasl: start sasl
erl -smp true +P 99999 -pa apps/*/ebin ./deps/*/ebin -boot start_sasl +K true -s proxy_client
