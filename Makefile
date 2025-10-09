all: dent.c
	$(CC) -Wall -Wextra -o dent dent.c

clean:
	rm -f dent
