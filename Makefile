##
## EPITECH PROJECT, 2020
## FUN_imageCompressor_2019
## File description:
## Makefile
##

NAME 	=	imageCompressor

all: $(NAME)

$(NAME):
	@stack build --copy-bins --local-bin-path .

clean:
	@stack clean --full

fclean: clean
	@rm -f $(NAME)

re: fclean all

debug:
	@stack ghci

profile:
	@stack build --copy-bins --local-bin-path . --profile

.PHONY: all clean fclean re debug
