PROJECT = toke

DEP_PLUGINS = rabbit_common/mk/rabbitmq-dist.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

CFLAGS += -I/usr/local/include
LDFLAGS += -L/usr/local/lib -ltokyocabinet
