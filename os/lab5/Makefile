MAKE = mingw32-make
SUBDIRS = init format

all: subdirs

subdirs:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir; \
	done