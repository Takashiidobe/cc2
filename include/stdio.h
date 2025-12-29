#ifndef _STDIO_H
#define _STDIO_H

/* NULL pointer constant */
#ifndef NULL
#define NULL ((void*)0)
#endif

/* size_t type - unsigned integer type of sizeof operator */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif

/* FILE type - opaque structure for file streams */
typedef struct _IO_FILE FILE;

/* End-of-file indicator */
#define EOF (-1)

/* Buffer size for setbuf */
#define BUFSIZ 8192

/* Maximum filename length */
#define FILENAME_MAX 4096

/* Maximum number of files that can be open simultaneously */
#define FOPEN_MAX 16

/* Size of buffer for tmpnam */
#define L_tmpnam 20

/* Seeking origins */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* Standard streams (extern declarations - defined by the C library) */
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

/* File operations */
extern FILE *fopen(const char *filename, const char *mode);
extern FILE *freopen(const char *filename, const char *mode, FILE *stream);
extern int fclose(FILE *stream);
extern int fflush(FILE *stream);
extern void setbuf(FILE *stream, char *buf);
extern int setvbuf(FILE *stream, char *buf, int mode, size_t size);

/* Formatted input/output */
extern int fprintf(FILE *stream, const char *format, ...);
extern int printf(const char *format, ...);
extern int sprintf(char *str, const char *format, ...);
extern int fscanf(FILE *stream, const char *format, ...);
extern int scanf(const char *format, ...);
extern int sscanf(const char *str, const char *format, ...);

/* Character input/output */
extern int fgetc(FILE *stream);
extern char *fgets(char *s, int size, FILE *stream);
extern int fputc(int c, FILE *stream);
extern int fputs(const char *s, FILE *stream);
extern int getc(FILE *stream);
extern int getchar(void);
extern char *gets(char *s);
extern int putc(int c, FILE *stream);
extern int putchar(int c);
extern int puts(const char *s);
extern int ungetc(int c, FILE *stream);

/* Direct input/output */
extern size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

/* File positioning */
extern int fseek(FILE *stream, long offset, int whence);
extern long ftell(FILE *stream);
extern void rewind(FILE *stream);

/* Error handling */
extern void clearerr(FILE *stream);
extern int feof(FILE *stream);
extern int ferror(FILE *stream);
extern void perror(const char *s);

/* File operations */
extern int remove(const char *filename);
extern int rename(const char *oldname, const char *newname);
extern FILE *tmpfile(void);
extern char *tmpnam(char *s);

#endif /* _STDIO_H */
