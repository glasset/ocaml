#* ************************************************************************** *#
#*                                                                            *#
#*                                                        :::      ::::::::   *#
#*   Makefile                                           :+:      :+:    :+:   *#
#*                                                    +:+ +:+         +:+     *#
#*   By: glasset <glasset@student.42.fr>            +#+  +:+       +#+        *#
#*                                                +#+#+#+#+#+   +#+           *#
#*   Created: 2016/10/07 09:53:01 by glasset           #+#    #+#             *#
#*   Updated: 2016/10/07 14:03:25 by glasset          ###   ########.fr       *#
#*                                                                            *#
#* ************************************************************************** *#

NAME = ft_crypt

SOURCES = cipher.ml \
		  uncipher.ml \
		  test.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = 

all: depend $(NAME)

$(NAME) : opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo . cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).byt
	rm -f $(NAME).opt

depend: .depend
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend
