ASM7090=/usr/local/src/emulators/7090/asm7090-2.2.2/asm7090
LNK7090=/usr/local/src/emulators/7090/lnk7090-2.1.3/lnk7090
UTILDIR=/usr/local/src/emulators/7090/utils-1.0.5
BCD2TXT=/usr/local/src/emulators/7090/s709/bcd2txt
S709=/usr/local/src/emulators/7090/s709/s709

all: lisp15.out
	@echo 'make run # to run it'

# We need to complete the lines to 80 characters,
# for asm7090 doesn't detect space opcode as PZE.
lisp15.src:Makefile lisp15.asm  head.card 
	cat head.card lisp15.asm \
	| sed \
		-e '{:loop' \
		-e 's/$$/              /' \
		-e 't next' \
		-e ':next' \
		-e 's/^\(.\{80\}\).*/\1/' \
		-e 't' \
		-e 'b loop}' \
	| sed \
		-e 's/PCC   /PCC ON/' \
		-e '/PAGE 121/s/SPACE   5/SPACE   4/' \
		-e '/PAGE 122/s/^       SPACE/*      SPACE/' \
		-e 's/^\([^ ]\+\) \+\<HED\>/       HEAD \1/' \
		-e 's/^\(.\{80\}\).*/\1/' \
	> lisp15.src

#
#		-e 's/^       TITLE/***    TITLE/' \
#		-e '/\* Local Variables:/a\* eval: (progn (widen)(put-computer-paper-overlay 2 2))' \
#
lisp15.out lisp15.lis:Makefile lisp15.src
	$(ASM7090) \
		-c 7090 \
		-a -f -p -w -x \
		-d LISP15 \
		-t 'BONNIE-S BIRTHDAY ASSEMBLY' \
		-l lisp15.lst \
		-o lisp15.out \
		lisp15.src || (	ls -l lisp15.*[a-z] ; exit 1)

TAP=lisp15.tape
TMP=core-dump.tape
PIT=punched-card-input.tape
POT=printed-output.tape
PPT=punched-card-output.tape
PUN=punch
PRI=print

run:lisp15.out
	$(UTILDIR)/obj2bin lisp15.out $(TAP)
	if [ -n $$DISPLAY ] ; then \
		xterm  -e \
        $(S709) -m7090 p=$(PRI).bcd u=$(PUN).bcd \
                       b7=$(TAP) b3=$(TMP) a2=$(PIT) a3=$(POT) a4=$(PPT) ;\
    else \
        $(S709) -m7090 p=$(PRI).bcd u=$(PUN).bcd \
                       b7=$(TAP) b3=$(TMP) a2=$(PIT) a3=$(POT) a4=$(PPT) ;\
	fi
	-$(BCD2TXT)    $(PUN).bcd $(PUN).txt 80 
	-$(BCD2TXT) -p $(PRI).bcd $(PRI).txt 132
	@echo '------------------------(punched cards)----------------------'
	-@cat $(PUN).txt
	@echo '-----------------------(printed listing)---------------------'
	-@cat $(PRI).txt
	@echo '-------------------------------------------------------------'

clean:
	-rm -f lisp15.out lisp15.lis lisp15.src 
	-rm -f $(TAP) $(TMP) $(PIT) $(POT) $(PPT) 
	-rm -f $(PRI).bcd $(PRI).txt $(PUN).bcd $(PUN).txt
	-rm -f printlog.lst

INSTALL_DIR=/larissa/data/local/html/informatimago/develop/lisp
URL_BASE=develop/lisp

archive $(INSTALL_DIR)/lisp15-$$VERSION.tar.gz:Makefile
	VERSION=$$(cat VERSION) ;\
	rm -rf lisp15-$$VERSION  ;\
	mkdir  lisp15-$$VERSION   ;\
	cp  README   \
		VERSION   \
		Makefile   \
		head.card   \
		lisp15.asm   \
		lisp15.lst    \
		lisp15.lisp    \
		asm7090.patch   \
        *.job            \
		/usr/local/users/pjb/src/public/emacs/pjb-page.el   \
		/usr/local/users/pjb/src/public/emacs/pjb-asm7090.el \
			lisp15-$$VERSION                           ;\
	chmod a+r lisp15-$$VERSION/*                        ;\
	tar zcf lisp15-$$VERSION.tar.gz lisp15-$$VERSION     ;\
	install -m 644 lisp15-$$VERSION.tar.gz $(INSTALL_DIR) ;\
	echo file:/$(INSTALL_DIR)/lisp15-$$VERSION.tar.gz      ;\
	echo http://www.informatimago.com/$(URL_BASE)/lisp15-$$VERSION.tar.gz
