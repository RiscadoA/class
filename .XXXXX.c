#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct record {
  void* cont;
  struct environment* env;
  unsigned char* read;
  unsigned char* write;
  int index;
  unsigned char polarity;
};

struct type_var {
  int size;
  unsigned char polarity;
};

struct task {
  void* cont;
  struct environment* env;
  struct task* next;
};

struct environment {
  struct environment* parent;
};

#define ENV_RECORD(env, i) (*(struct record**)(((unsigned char*)env) + sizeof(struct environment) + sizeof(struct record*) * i))
#define ENV_TYPE_VAR(env, r, i) (*(struct type_var*)(((unsigned char*)env) + sizeof(struct environment) + sizeof(struct record*) * r + sizeof(struct type_var) * i))

int main() {
  struct record* tmp_session;
  struct environment* env = NULL;
  struct environment* tmp_env;
  void* tmp_cont;
  struct task* next_task = malloc(sizeof(struct task));
  next_task->cont = &&end;
  struct task* tmp_task;

  goto run;

proc_zero:
  /* BEGIN unfold-send(n_1):-1 */
    /* BEGIN flip(n_1):unfold-send */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_0;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_0:
    /* END flip(n_1):unfold-send */
    ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    /* BEGIN select#Z(n_1):9 */
      *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 1 /* #Z */;
      ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
      /* BEGIN close(n_1):10 */
        /* BEGIN finalFlip(n_1) */
          tmp_session = ENV_RECORD(env, 0 /*n_1*/);
          env = tmp_session->env;
          goto *tmp_session->cont;
        /* END finalFlip(n_1) */
      /* END close(n_1):10 */
    /* END select#Z(n_1):9 */
  /* END unfold-send(n_1):-1 */

proc_printNat:
  if (ENV_RECORD(env, 0 /*n_1*/)->polarity) {
    /* BEGIN flip(n_1):polarity */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_1;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_1:
    /* END flip(n_1):polarity */
  } else {
  }
  /* BEGIN unfold-recv(n_1):-1 */
    /* BEGIN flip(n_1):unfold-recv */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_2;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_2:
    /* END flip(n_1):unfold-recv */
    ENV_RECORD(env, 0 /*n_1*/)->read = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    /* BEGIN case(n_1):-1 */
      switch (*((unsigned char*)((ENV_RECORD(env, 0 /*n_1*/)->read += sizeof(unsigned char)) - sizeof(unsigned char)))) {
        case 0:
          /* BEGIN case#S(n_1):-1 */
            /* BEGIN println:-1 */
              fputs("S ", stdout);
              /* BEGIN id(printNat):16 */
                tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
                ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
                env = tmp_env;
                goto proc_printNat;
              /* END id(printNat):16 */
            /* END println:-1 */
          /* END case#S(n_1):-1 */
        case 1:
          /* BEGIN case#Z(n_1):-1 */
            /* BEGIN println:-1 */
              fputs("Z\n", stdout);
              /* BEGIN coclose(n_1):-1 */
                free(ENV_RECORD(env, 0 /*n_1*/));
                /* BEGIN empty:15 */
                  tmp_task = next_task->next;
                  env = next_task->env;
                  tmp_cont = next_task->cont;
                  free(next_task);
                  next_task = tmp_task;
                  goto *tmp_cont;
                /* END empty:15 */
              /* END coclose(n_1):-1 */
            /* END println:-1 */
          /* END case#Z(n_1):-1 */
      }
    /* END case(n_1):-1 */
  /* END unfold-recv(n_1):-1 */

proc_add:
  if (ENV_RECORD(env, 0 /*n_1*/)->polarity) {
    /* BEGIN flip(n_1):polarity */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_3;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_3:
    /* END flip(n_1):polarity */
  } else {
  }
  /* BEGIN unfold-recv(n_1):-1 */
    /* BEGIN flip(n_1):unfold-recv */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_4;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_4:
    /* END flip(n_1):unfold-recv */
    ENV_RECORD(env, 0 /*n_1*/)->read = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    /* BEGIN case(n_1):-1 */
      switch (*((unsigned char*)((ENV_RECORD(env, 0 /*n_1*/)->read += sizeof(unsigned char)) - sizeof(unsigned char)))) {
        case 0:
          /* BEGIN case#S(n_1):-1 */
            /* BEGIN unfold-send(r_1):-1 */
              /* BEGIN flip(r_1):unfold-send */
                tmp_env = ENV_RECORD(env, 2 /*r_1*/)->env;
                tmp_cont = ENV_RECORD(env, 2 /*r_1*/)->cont;
                ENV_RECORD(env, 2 /*r_1*/)->cont = &&flip_r_1_lbl_5;
                ENV_RECORD(env, 2 /*r_1*/)->env = env;
                ENV_RECORD(env, 2 /*r_1*/)->index = 2;
                env = tmp_env;
                goto *tmp_cont;
flip_r_1_lbl_5:
              /* END flip(r_1):unfold-send */
              ENV_RECORD(env, 2 /*r_1*/)->write = ((unsigned char*)ENV_RECORD(env, 2 /*r_1*/) + sizeof(struct record));
              /* BEGIN select#S(r_1):23 */
                *(unsigned char*)ENV_RECORD(env, 2 /*r_1*/)->write = 0 /* #S */;
                ENV_RECORD(env, 2 /*r_1*/)->write += sizeof(unsigned char);
                /* BEGIN id(add):23 */
                  tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (3) + sizeof(struct type_var) * (0));
                  ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
                  ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
                  ENV_RECORD(tmp_env, 1 /*m_1*/) = ENV_RECORD(env, 1 /*m_1*/);
                  ENV_RECORD(tmp_env, 2 /*r_1*/) = ENV_RECORD(env, 2 /*r_1*/);
                  ENV_RECORD(env, 2 /*r_1*/)->polarity = 1;
                  env = tmp_env;
                  goto proc_add;
                /* END id(add):23 */
              /* END select#S(r_1):23 */
            /* END unfold-send(r_1):-1 */
          /* END case#S(n_1):-1 */
        case 1:
          /* BEGIN case#Z(n_1):-1 */
            /* BEGIN coclose(n_1):-1 */
              free(ENV_RECORD(env, 0 /*n_1*/));
              /* BEGIN fwd(m_1, r_1):22 */
                if (ENV_RECORD(env, 1 /*m_1*/)->polarity) {
                  tmp_env = ENV_RECORD(env, 1 /*m_1*/)->env;
                  tmp_cont = ENV_RECORD(env, 1 /*m_1*/)->cont;
                  if (ENV_RECORD(env, 2 /*r_1*/)->polarity) {
                    memcpy(ENV_RECORD(env, 1 /*m_1*/)->write, ENV_RECORD(env, 2 /*r_1*/)->read, ENV_RECORD(env, 2 /*r_1*/)->write - ENV_RECORD(env, 2 /*r_1*/)->read);
                    ENV_RECORD(env, 1 /*m_1*/)->write += ENV_RECORD(env, 2 /*r_1*/)->write - ENV_RECORD(env, 2 /*r_1*/)->read;
                  } else {
                  }
                  ENV_RECORD(env, 1 /*m_1*/)->env = ENV_RECORD(env, 2 /*r_1*/)->env;
                  ENV_RECORD(env, 1 /*m_1*/)->cont = ENV_RECORD(env, 2 /*r_1*/)->cont;
                  ENV_RECORD(env, 1 /*m_1*/)->index = ENV_RECORD(env, 2 /*r_1*/)->index;
                  tmp_session = ENV_RECORD(env, 2 /*r_1*/);
                  ENV_RECORD(ENV_RECORD(env, 2 /*r_1*/)->env, ENV_RECORD(env, 2 /*r_1*/)->index) = ENV_RECORD(env, 1 /*m_1*/);
                  free(tmp_session);
                } else {
                  tmp_env = ENV_RECORD(env, 2 /*r_1*/)->env;
                  tmp_cont = ENV_RECORD(env, 2 /*r_1*/)->cont;
                  if (ENV_RECORD(env, 2 /*r_1*/)->polarity) {
                    memcpy(ENV_RECORD(env, 2 /*r_1*/)->write, ENV_RECORD(env, 1 /*m_1*/)->read, ENV_RECORD(env, 1 /*m_1*/)->write - ENV_RECORD(env, 1 /*m_1*/)->read);
                    ENV_RECORD(env, 2 /*r_1*/)->write += ENV_RECORD(env, 1 /*m_1*/)->write - ENV_RECORD(env, 1 /*m_1*/)->read;
                    ENV_RECORD(env, 2 /*r_1*/)->env = ENV_RECORD(env, 1 /*m_1*/)->env;
                    ENV_RECORD(env, 2 /*r_1*/)->cont = ENV_RECORD(env, 1 /*m_1*/)->cont;
                    ENV_RECORD(env, 2 /*r_1*/)->index = ENV_RECORD(env, 1 /*m_1*/)->index;
                    tmp_session = ENV_RECORD(env, 1 /*m_1*/);
                    ENV_RECORD(ENV_RECORD(env, 1 /*m_1*/)->env, ENV_RECORD(env, 1 /*m_1*/)->index) = ENV_RECORD(env, 2 /*r_1*/);
                    free(tmp_session);
                  } else {
                    tmp_session = ENV_RECORD(env, 2 /*r_1*/);
                    ENV_RECORD(ENV_RECORD(env, 2 /*r_1*/)->env, ENV_RECORD(env, 2 /*r_1*/)->index) = ENV_RECORD(env, 1 /*m_1*/);
                    free(tmp_session);
                  }
                }
                env = tmp_env;
                goto *tmp_cont;
              /* END fwd(m_1, r_1):22 */
            /* END coclose(n_1):-1 */
          /* END case#Z(n_1):-1 */
      }
    /* END case(n_1):-1 */
  /* END unfold-recv(n_1):-1 */

proc_drop:
  if (ENV_RECORD(env, 0 /*n_1*/)->polarity) {
    /* BEGIN flip(n_1):polarity */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_6;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_6:
    /* END flip(n_1):polarity */
  } else {
  }
  /* BEGIN unfold-recv(n_1):-1 */
    /* BEGIN flip(n_1):unfold-recv */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_7;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_7:
    /* END flip(n_1):unfold-recv */
    ENV_RECORD(env, 0 /*n_1*/)->read = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    /* BEGIN case(n_1):-1 */
      switch (*((unsigned char*)((ENV_RECORD(env, 0 /*n_1*/)->read += sizeof(unsigned char)) - sizeof(unsigned char)))) {
        case 0:
          /* BEGIN case#S(n_1):-1 */
            /* BEGIN id(drop):33 */
              tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
              ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
              ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
              env = tmp_env;
              goto proc_drop;
            /* END id(drop):33 */
          /* END case#S(n_1):-1 */
        case 1:
          /* BEGIN case#Z(n_1):-1 */
            /* BEGIN coclose(n_1):-1 */
              free(ENV_RECORD(env, 0 /*n_1*/));
              /* BEGIN empty:32 */
                tmp_task = next_task->next;
                env = next_task->env;
                tmp_cont = next_task->cont;
                free(next_task);
                next_task = tmp_task;
                goto *tmp_cont;
              /* END empty:32 */
            /* END coclose(n_1):-1 */
          /* END case#Z(n_1):-1 */
      }
    /* END case(n_1):-1 */
  /* END unfold-recv(n_1):-1 */

proc_copy:
  if (ENV_RECORD(env, 0 /*n_1*/)->polarity) {
    /* BEGIN flip(n_1):polarity */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_8;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_8:
    /* END flip(n_1):polarity */
  } else {
  }
  /* BEGIN unfold-recv(n_1):-1 */
    /* BEGIN flip(n_1):unfold-recv */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_9;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_9:
    /* END flip(n_1):unfold-recv */
    ENV_RECORD(env, 0 /*n_1*/)->read = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    /* BEGIN case(n_1):-1 */
      switch (*((unsigned char*)((ENV_RECORD(env, 0 /*n_1*/)->read += sizeof(unsigned char)) - sizeof(unsigned char)))) {
        case 0:
          /* BEGIN case#S(n_1):-1 */
            /* BEGIN cut(d0_1):-1 */
              ENV_RECORD(env, 2 /*d0_1*/) = malloc(sizeof(struct record) + (sizeof(struct record*) + sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
              ENV_RECORD(env, 2 /*d0_1*/)->read = ENV_RECORD(env, 2 /*d0_1*/)->write = ((unsigned char*)ENV_RECORD(env, 2 /*d0_1*/) + sizeof(struct record));
              ENV_RECORD(env, 2 /*d0_1*/)->cont = &&cut_d0_1_lbl_10;
              ENV_RECORD(env, 2 /*d0_1*/)->env = env;
              ENV_RECORD(env, 2 /*d0_1*/)->index = 2;
              /* BEGIN id(copy):42 */
                tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (6) + sizeof(struct type_var) * (0));
                ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
                ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
                ENV_RECORD(tmp_env, 1 /*d_1*/) = ENV_RECORD(env, 2 /*d0_1*/);
                ENV_RECORD(env, 2 /*d0_1*/)->polarity = 1;
                env = tmp_env;
                goto proc_copy;
              /* END id(copy):42 */
cut_d0_1_lbl_10:
              /* BEGIN recv(d0_1, c0_1):-1 */
                ENV_RECORD(env, 3 /*c0_1*/) = *((struct record**)((ENV_RECORD(env, 2 /*d0_1*/)->read += sizeof(struct record*)) - sizeof(struct record*)));
                /* BEGIN send(d_1):45 */
                  ENV_RECORD(env, 4 /*s_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
                  ENV_RECORD(env, 4 /*s_1*/)->read = ENV_RECORD(env, 4 /*s_1*/)->write = ((unsigned char*)ENV_RECORD(env, 4 /*s_1*/) + sizeof(struct record));
                  ENV_RECORD(env, 4 /*s_1*/)->cont = &&send_d_1_s_1_lhs_lbl_11;
                  ENV_RECORD(env, 4 /*s_1*/)->env = env;
                  ENV_RECORD(env, 4 /*s_1*/)->index = 4;
                  *(struct record**)ENV_RECORD(env, 1 /*d_1*/)->write = ENV_RECORD(env, 4 /*s_1*/);
                  ENV_RECORD(env, 1 /*d_1*/)->write += sizeof(struct record*);
                  /* BEGIN closure(s_1):45 */
                    goto send_d_1_s_1_rhs_lbl_12;
send_d_1_s_1_lhs_lbl_11:
                    /* BEGIN unfold-send(s_1):-1 */
                      /* BEGIN flip(s_1):unfold-send */
                        tmp_env = ENV_RECORD(env, 4 /*s_1*/)->env;
                        tmp_cont = ENV_RECORD(env, 4 /*s_1*/)->cont;
                        ENV_RECORD(env, 4 /*s_1*/)->cont = &&flip_s_1_lbl_13;
                        ENV_RECORD(env, 4 /*s_1*/)->env = env;
                        ENV_RECORD(env, 4 /*s_1*/)->index = 4;
                        env = tmp_env;
                        goto *tmp_cont;
flip_s_1_lbl_13:
                      /* END flip(s_1):unfold-send */
                      ENV_RECORD(env, 4 /*s_1*/)->write = ((unsigned char*)ENV_RECORD(env, 4 /*s_1*/) + sizeof(struct record));
                      /* BEGIN select#S(s_1):45 */
                        *(unsigned char*)ENV_RECORD(env, 4 /*s_1*/)->write = 0 /* #S */;
                        ENV_RECORD(env, 4 /*s_1*/)->write += sizeof(unsigned char);
                        /* BEGIN fwd(c0_1, s_1):45 */
                          tmp_env = ENV_RECORD(env, 3 /*c0_1*/)->env;
                          tmp_cont = ENV_RECORD(env, 3 /*c0_1*/)->cont;
                          memcpy(ENV_RECORD(env, 3 /*c0_1*/)->write, ENV_RECORD(env, 4 /*s_1*/)->read, ENV_RECORD(env, 4 /*s_1*/)->write - ENV_RECORD(env, 4 /*s_1*/)->read);
                          ENV_RECORD(env, 3 /*c0_1*/)->write += ENV_RECORD(env, 4 /*s_1*/)->write - ENV_RECORD(env, 4 /*s_1*/)->read;
                          ENV_RECORD(env, 3 /*c0_1*/)->env = ENV_RECORD(env, 4 /*s_1*/)->env;
                          ENV_RECORD(env, 3 /*c0_1*/)->cont = ENV_RECORD(env, 4 /*s_1*/)->cont;
                          ENV_RECORD(env, 3 /*c0_1*/)->index = ENV_RECORD(env, 4 /*s_1*/)->index;
                          tmp_session = ENV_RECORD(env, 4 /*s_1*/);
                          ENV_RECORD(ENV_RECORD(env, 4 /*s_1*/)->env, ENV_RECORD(env, 4 /*s_1*/)->index) = ENV_RECORD(env, 3 /*c0_1*/);
                          free(tmp_session);
                          env = tmp_env;
                          goto *tmp_cont;
                        /* END fwd(c0_1, s_1):45 */
                      /* END select#S(s_1):45 */
                    /* END unfold-send(s_1):-1 */
                  /* END closure(s_1):45 */
send_d_1_s_1_rhs_lbl_12:
                  /* BEGIN unfold-send(d_1):-1 */
                    /* BEGIN flip(d_1):unfold-send */
                      tmp_env = ENV_RECORD(env, 1 /*d_1*/)->env;
                      tmp_cont = ENV_RECORD(env, 1 /*d_1*/)->cont;
                      ENV_RECORD(env, 1 /*d_1*/)->cont = &&flip_d_1_lbl_14;
                      ENV_RECORD(env, 1 /*d_1*/)->env = env;
                      ENV_RECORD(env, 1 /*d_1*/)->index = 1;
                      env = tmp_env;
                      goto *tmp_cont;
flip_d_1_lbl_14:
                    /* END flip(d_1):unfold-send */
                    ENV_RECORD(env, 1 /*d_1*/)->write = ((unsigned char*)ENV_RECORD(env, 1 /*d_1*/) + sizeof(struct record));
                    /* BEGIN select#S(d_1):46 */
                      *(unsigned char*)ENV_RECORD(env, 1 /*d_1*/)->write = 0 /* #S */;
                      ENV_RECORD(env, 1 /*d_1*/)->write += sizeof(unsigned char);
                      /* BEGIN fwd(d0_1, d_1):46 */
                        tmp_env = ENV_RECORD(env, 1 /*d_1*/)->env;
                        tmp_cont = ENV_RECORD(env, 1 /*d_1*/)->cont;
                        memcpy(ENV_RECORD(env, 1 /*d_1*/)->write, ENV_RECORD(env, 2 /*d0_1*/)->read, ENV_RECORD(env, 2 /*d0_1*/)->write - ENV_RECORD(env, 2 /*d0_1*/)->read);
                        ENV_RECORD(env, 1 /*d_1*/)->write += ENV_RECORD(env, 2 /*d0_1*/)->write - ENV_RECORD(env, 2 /*d0_1*/)->read;
                        ENV_RECORD(env, 1 /*d_1*/)->env = ENV_RECORD(env, 2 /*d0_1*/)->env;
                        ENV_RECORD(env, 1 /*d_1*/)->cont = ENV_RECORD(env, 2 /*d0_1*/)->cont;
                        ENV_RECORD(env, 1 /*d_1*/)->index = ENV_RECORD(env, 2 /*d0_1*/)->index;
                        tmp_session = ENV_RECORD(env, 2 /*d0_1*/);
                        ENV_RECORD(ENV_RECORD(env, 2 /*d0_1*/)->env, ENV_RECORD(env, 2 /*d0_1*/)->index) = ENV_RECORD(env, 1 /*d_1*/);
                        free(tmp_session);
                        env = tmp_env;
                        goto *tmp_cont;
                      /* END fwd(d0_1, d_1):46 */
                    /* END select#S(d_1):46 */
                  /* END unfold-send(d_1):-1 */
                /* END send(d_1):45 */
              /* END recv(d0_1, c0_1):-1 */
            /* END cut(d0_1):-1 */
          /* END case#S(n_1):-1 */
        case 1:
          /* BEGIN case#Z(n_1):-1 */
            /* BEGIN coclose(n_1):-1 */
              free(ENV_RECORD(env, 0 /*n_1*/));
              /* BEGIN send(d_1):40 */
                ENV_RECORD(env, 5 /*z_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
                ENV_RECORD(env, 5 /*z_1*/)->read = ENV_RECORD(env, 5 /*z_1*/)->write = ((unsigned char*)ENV_RECORD(env, 5 /*z_1*/) + sizeof(struct record));
                ENV_RECORD(env, 5 /*z_1*/)->cont = &&send_d_1_z_1_lhs_lbl_15;
                ENV_RECORD(env, 5 /*z_1*/)->env = env;
                ENV_RECORD(env, 5 /*z_1*/)->index = 5;
                *(struct record**)ENV_RECORD(env, 1 /*d_1*/)->write = ENV_RECORD(env, 5 /*z_1*/);
                ENV_RECORD(env, 1 /*d_1*/)->write += sizeof(struct record*);
                /* BEGIN closure(z_1):40 */
                  goto send_d_1_z_1_rhs_lbl_16;
send_d_1_z_1_lhs_lbl_15:
                  /* BEGIN id(zero):40 */
                    tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                    ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 5 /*z_1*/);
                    ENV_RECORD(env, 5 /*z_1*/)->polarity = 0;
                    env = tmp_env;
                    goto proc_zero;
                  /* END id(zero):40 */
                /* END closure(z_1):40 */
send_d_1_z_1_rhs_lbl_16:
                /* BEGIN id(zero):40 */
                  tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                  ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 1 /*d_1*/);
                  ENV_RECORD(env, 1 /*d_1*/)->polarity = 1;
                  env = tmp_env;
                  goto proc_zero;
                /* END id(zero):40 */
              /* END send(d_1):40 */
            /* END coclose(n_1):-1 */
          /* END case#Z(n_1):-1 */
      }
    /* END case(n_1):-1 */
  /* END unfold-recv(n_1):-1 */

proc_mul:
  if (ENV_RECORD(env, 0 /*n_1*/)->polarity) {
    /* BEGIN flip(n_1):polarity */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_17;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_17:
    /* END flip(n_1):polarity */
  } else {
  }
  /* BEGIN unfold-recv(n_1):-1 */
    /* BEGIN flip(n_1):unfold-recv */
      tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
      tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
      ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_18;
      ENV_RECORD(env, 0 /*n_1*/)->env = env;
      ENV_RECORD(env, 0 /*n_1*/)->index = 0;
      env = tmp_env;
      goto *tmp_cont;
flip_n_1_lbl_18:
    /* END flip(n_1):unfold-recv */
    ENV_RECORD(env, 0 /*n_1*/)->read = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    /* BEGIN case(n_1):-1 */
      switch (*((unsigned char*)((ENV_RECORD(env, 0 /*n_1*/)->read += sizeof(unsigned char)) - sizeof(unsigned char)))) {
        case 0:
          /* BEGIN case#S(n_1):-1 */
            /* BEGIN cut(d0_1):-1 */
              ENV_RECORD(env, 3 /*d0_1*/) = malloc(sizeof(struct record) + (sizeof(struct record*) + sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
              ENV_RECORD(env, 3 /*d0_1*/)->read = ENV_RECORD(env, 3 /*d0_1*/)->write = ((unsigned char*)ENV_RECORD(env, 3 /*d0_1*/) + sizeof(struct record));
              ENV_RECORD(env, 3 /*d0_1*/)->cont = &&cut_d0_1_lbl_19;
              ENV_RECORD(env, 3 /*d0_1*/)->env = env;
              ENV_RECORD(env, 3 /*d0_1*/)->index = 3;
              /* BEGIN id(copy):55 */
                tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (6) + sizeof(struct type_var) * (0));
                ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 1 /*m_1*/);
                ENV_RECORD(tmp_env, 1 /*d_1*/) = ENV_RECORD(env, 3 /*d0_1*/);
                ENV_RECORD(env, 3 /*d0_1*/)->polarity = 1;
                env = tmp_env;
                goto proc_copy;
              /* END id(copy):55 */
cut_d0_1_lbl_19:
              /* BEGIN recv(d0_1, m0_1):-1 */
                ENV_RECORD(env, 4 /*m0_1*/) = *((struct record**)((ENV_RECORD(env, 3 /*d0_1*/)->read += sizeof(struct record*)) - sizeof(struct record*)));
                /* BEGIN cut(r0_1):-1 */
                  ENV_RECORD(env, 5 /*r0_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
                  ENV_RECORD(env, 5 /*r0_1*/)->read = ENV_RECORD(env, 5 /*r0_1*/)->write = ((unsigned char*)ENV_RECORD(env, 5 /*r0_1*/) + sizeof(struct record));
                  ENV_RECORD(env, 5 /*r0_1*/)->cont = &&cut_r0_1_lbl_20;
                  ENV_RECORD(env, 5 /*r0_1*/)->env = env;
                  ENV_RECORD(env, 5 /*r0_1*/)->index = 5;
                  /* BEGIN id(mul):59 */
                    tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (6) + sizeof(struct type_var) * (0));
                    ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
                    ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
                    ENV_RECORD(tmp_env, 1 /*m_1*/) = ENV_RECORD(env, 4 /*m0_1*/);
                    ENV_RECORD(env, 4 /*m0_1*/)->polarity = 1;
                    ENV_RECORD(tmp_env, 2 /*r_1*/) = ENV_RECORD(env, 5 /*r0_1*/);
                    ENV_RECORD(env, 5 /*r0_1*/)->polarity = 1;
                    env = tmp_env;
                    goto proc_mul;
                  /* END id(mul):59 */
cut_r0_1_lbl_20:
                  /* BEGIN id(add):61 */
                    tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (3) + sizeof(struct type_var) * (0));
                    ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 3 /*d0_1*/);
                    ENV_RECORD(env, 3 /*d0_1*/)->polarity = 0;
                    ENV_RECORD(tmp_env, 1 /*m_1*/) = ENV_RECORD(env, 5 /*r0_1*/);
                    ENV_RECORD(env, 5 /*r0_1*/)->polarity = 0;
                    ENV_RECORD(tmp_env, 2 /*r_1*/) = ENV_RECORD(env, 2 /*r_1*/);
                    env = tmp_env;
                    goto proc_add;
                  /* END id(add):61 */
                /* END cut(r0_1):-1 */
              /* END recv(d0_1, m0_1):-1 */
            /* END cut(d0_1):-1 */
          /* END case#S(n_1):-1 */
        case 1:
          /* BEGIN case#Z(n_1):-1 */
            /* BEGIN coclose(n_1):-1 */
              free(ENV_RECORD(env, 0 /*n_1*/));
              /* BEGIN unfold-send(r_1):-1 */
                /* BEGIN flip(r_1):unfold-send */
                  tmp_env = ENV_RECORD(env, 2 /*r_1*/)->env;
                  tmp_cont = ENV_RECORD(env, 2 /*r_1*/)->cont;
                  ENV_RECORD(env, 2 /*r_1*/)->cont = &&flip_r_1_lbl_21;
                  ENV_RECORD(env, 2 /*r_1*/)->env = env;
                  ENV_RECORD(env, 2 /*r_1*/)->index = 2;
                  env = tmp_env;
                  goto *tmp_cont;
flip_r_1_lbl_21:
                /* END flip(r_1):unfold-send */
                ENV_RECORD(env, 2 /*r_1*/)->write = ((unsigned char*)ENV_RECORD(env, 2 /*r_1*/) + sizeof(struct record));
                /* BEGIN select#Z(r_1):53 */
                  *(unsigned char*)ENV_RECORD(env, 2 /*r_1*/)->write = 1 /* #Z */;
                  ENV_RECORD(env, 2 /*r_1*/)->write += sizeof(unsigned char);
                  /* BEGIN mix:-1 */
                    tmp_task = malloc(sizeof(struct task));
                    tmp_task->cont = &&mix_rhs_lbl_22;
                    tmp_task->env = env;
                    tmp_task->next = next_task;
                    next_task = tmp_task;
                    /* BEGIN id(drop):53 */
                      tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                      ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 1 /*m_1*/);
                      env = tmp_env;
                      goto proc_drop;
                    /* END id(drop):53 */
mix_rhs_lbl_22:
                    /* BEGIN close(r_1):53 */
                      /* BEGIN finalFlip(r_1) */
                        tmp_session = ENV_RECORD(env, 2 /*r_1*/);
                        env = tmp_session->env;
                        goto *tmp_session->cont;
                      /* END finalFlip(r_1) */
                    /* END close(r_1):53 */
                  /* END mix:-1 */
                /* END select#Z(r_1):53 */
              /* END unfold-send(r_1):-1 */
            /* END coclose(n_1):-1 */
          /* END case#Z(n_1):-1 */
      }
    /* END case(n_1):-1 */
  /* END unfold-recv(n_1):-1 */

proc_main1:
  /* BEGIN cut(n_1):-1 */
    ENV_RECORD(env, 0 /*n_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
    ENV_RECORD(env, 0 /*n_1*/)->read = ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    ENV_RECORD(env, 0 /*n_1*/)->cont = &&cut_n_1_lbl_23;
    ENV_RECORD(env, 0 /*n_1*/)->env = env;
    ENV_RECORD(env, 0 /*n_1*/)->index = 0;
    /* BEGIN unfold-send(n_1):-1 */
      /* BEGIN flip(n_1):unfold-send */
        tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
        tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
        ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_24;
        ENV_RECORD(env, 0 /*n_1*/)->env = env;
        ENV_RECORD(env, 0 /*n_1*/)->index = 0;
        env = tmp_env;
        goto *tmp_cont;
flip_n_1_lbl_24:
      /* END flip(n_1):unfold-send */
      ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
      /* BEGIN select#S(n_1):69 */
        *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
        ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
        /* BEGIN unfold-send(n_1):-1 */
          /* BEGIN flip(n_1):unfold-send */
            tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
            tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
            ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_25;
            ENV_RECORD(env, 0 /*n_1*/)->env = env;
            ENV_RECORD(env, 0 /*n_1*/)->index = 0;
            env = tmp_env;
            goto *tmp_cont;
flip_n_1_lbl_25:
          /* END flip(n_1):unfold-send */
          ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
          /* BEGIN select#S(n_1):69 */
            *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
            ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
            /* BEGIN id(zero):69 */
              tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
              ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
              ENV_RECORD(env, 0 /*n_1*/)->polarity = 1;
              env = tmp_env;
              goto proc_zero;
            /* END id(zero):69 */
          /* END select#S(n_1):69 */
        /* END unfold-send(n_1):-1 */
      /* END select#S(n_1):69 */
    /* END unfold-send(n_1):-1 */
cut_n_1_lbl_23:
    /* BEGIN id(printNat):71 */
      tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
      ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
      ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
      env = tmp_env;
      goto proc_printNat;
    /* END id(printNat):71 */
  /* END cut(n_1):-1 */

proc_main2:
  /* BEGIN cut(n_1):-1 */
    ENV_RECORD(env, 0 /*n_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
    ENV_RECORD(env, 0 /*n_1*/)->read = ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    ENV_RECORD(env, 0 /*n_1*/)->cont = &&cut_n_1_lbl_26;
    ENV_RECORD(env, 0 /*n_1*/)->env = env;
    ENV_RECORD(env, 0 /*n_1*/)->index = 0;
    /* BEGIN unfold-send(n_1):-1 */
      /* BEGIN flip(n_1):unfold-send */
        tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
        tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
        ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_27;
        ENV_RECORD(env, 0 /*n_1*/)->env = env;
        ENV_RECORD(env, 0 /*n_1*/)->index = 0;
        env = tmp_env;
        goto *tmp_cont;
flip_n_1_lbl_27:
      /* END flip(n_1):unfold-send */
      ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
      /* BEGIN select#S(n_1):77 */
        *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
        ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
        /* BEGIN unfold-send(n_1):-1 */
          /* BEGIN flip(n_1):unfold-send */
            tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
            tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
            ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_28;
            ENV_RECORD(env, 0 /*n_1*/)->env = env;
            ENV_RECORD(env, 0 /*n_1*/)->index = 0;
            env = tmp_env;
            goto *tmp_cont;
flip_n_1_lbl_28:
          /* END flip(n_1):unfold-send */
          ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
          /* BEGIN select#S(n_1):77 */
            *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
            ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
            /* BEGIN unfold-send(n_1):-1 */
              /* BEGIN flip(n_1):unfold-send */
                tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
                tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
                ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_29;
                ENV_RECORD(env, 0 /*n_1*/)->env = env;
                ENV_RECORD(env, 0 /*n_1*/)->index = 0;
                env = tmp_env;
                goto *tmp_cont;
flip_n_1_lbl_29:
              /* END flip(n_1):unfold-send */
              ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
              /* BEGIN select#S(n_1):77 */
                *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
                ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
                /* BEGIN unfold-send(n_1):-1 */
                  /* BEGIN flip(n_1):unfold-send */
                    tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
                    tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
                    ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_30;
                    ENV_RECORD(env, 0 /*n_1*/)->env = env;
                    ENV_RECORD(env, 0 /*n_1*/)->index = 0;
                    env = tmp_env;
                    goto *tmp_cont;
flip_n_1_lbl_30:
                  /* END flip(n_1):unfold-send */
                  ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
                  /* BEGIN select#S(n_1):77 */
                    *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
                    ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
                    /* BEGIN id(zero):77 */
                      tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                      ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
                      ENV_RECORD(env, 0 /*n_1*/)->polarity = 1;
                      env = tmp_env;
                      goto proc_zero;
                    /* END id(zero):77 */
                  /* END select#S(n_1):77 */
                /* END unfold-send(n_1):-1 */
              /* END select#S(n_1):77 */
            /* END unfold-send(n_1):-1 */
          /* END select#S(n_1):77 */
        /* END unfold-send(n_1):-1 */
      /* END select#S(n_1):77 */
    /* END unfold-send(n_1):-1 */
cut_n_1_lbl_26:
    /* BEGIN cut(d0_1):-1 */
      ENV_RECORD(env, 1 /*d0_1*/) = malloc(sizeof(struct record) + (sizeof(struct record*) + sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
      ENV_RECORD(env, 1 /*d0_1*/)->read = ENV_RECORD(env, 1 /*d0_1*/)->write = ((unsigned char*)ENV_RECORD(env, 1 /*d0_1*/) + sizeof(struct record));
      ENV_RECORD(env, 1 /*d0_1*/)->cont = &&cut_d0_1_lbl_31;
      ENV_RECORD(env, 1 /*d0_1*/)->env = env;
      ENV_RECORD(env, 1 /*d0_1*/)->index = 1;
      /* BEGIN id(copy):79 */
        tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (6) + sizeof(struct type_var) * (0));
        ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
        ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
        ENV_RECORD(tmp_env, 1 /*d_1*/) = ENV_RECORD(env, 1 /*d0_1*/);
        ENV_RECORD(env, 1 /*d0_1*/)->polarity = 1;
        env = tmp_env;
        goto proc_copy;
      /* END id(copy):79 */
cut_d0_1_lbl_31:
      /* BEGIN recv(d0_1, n0_1):-1 */
        ENV_RECORD(env, 2 /*n0_1*/) = *((struct record**)((ENV_RECORD(env, 1 /*d0_1*/)->read += sizeof(struct record*)) - sizeof(struct record*)));
        /* BEGIN mix:-1 */
          tmp_task = malloc(sizeof(struct task));
          tmp_task->cont = &&mix_rhs_lbl_32;
          tmp_task->env = env;
          tmp_task->next = next_task;
          next_task = tmp_task;
          /* BEGIN id(printNat):82 */
            tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
            ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 2 /*n0_1*/);
            ENV_RECORD(env, 2 /*n0_1*/)->polarity = 1;
            env = tmp_env;
            goto proc_printNat;
          /* END id(printNat):82 */
mix_rhs_lbl_32:
          /* BEGIN id(printNat):82 */
            tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
            ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 1 /*d0_1*/);
            ENV_RECORD(env, 1 /*d0_1*/)->polarity = 0;
            env = tmp_env;
            goto proc_printNat;
          /* END id(printNat):82 */
        /* END mix:-1 */
      /* END recv(d0_1, n0_1):-1 */
    /* END cut(d0_1):-1 */
  /* END cut(n_1):-1 */

proc_main:
  /* BEGIN cut(n_1):-1 */
    ENV_RECORD(env, 0 /*n_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
    ENV_RECORD(env, 0 /*n_1*/)->read = ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
    ENV_RECORD(env, 0 /*n_1*/)->cont = &&cut_n_1_lbl_33;
    ENV_RECORD(env, 0 /*n_1*/)->env = env;
    ENV_RECORD(env, 0 /*n_1*/)->index = 0;
    /* BEGIN unfold-send(n_1):-1 */
      /* BEGIN flip(n_1):unfold-send */
        tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
        tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
        ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_34;
        ENV_RECORD(env, 0 /*n_1*/)->env = env;
        ENV_RECORD(env, 0 /*n_1*/)->index = 0;
        env = tmp_env;
        goto *tmp_cont;
flip_n_1_lbl_34:
      /* END flip(n_1):unfold-send */
      ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
      /* BEGIN select#S(n_1):88 */
        *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
        ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
        /* BEGIN unfold-send(n_1):-1 */
          /* BEGIN flip(n_1):unfold-send */
            tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
            tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
            ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_35;
            ENV_RECORD(env, 0 /*n_1*/)->env = env;
            ENV_RECORD(env, 0 /*n_1*/)->index = 0;
            env = tmp_env;
            goto *tmp_cont;
flip_n_1_lbl_35:
          /* END flip(n_1):unfold-send */
          ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
          /* BEGIN select#S(n_1):88 */
            *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
            ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
            /* BEGIN unfold-send(n_1):-1 */
              /* BEGIN flip(n_1):unfold-send */
                tmp_env = ENV_RECORD(env, 0 /*n_1*/)->env;
                tmp_cont = ENV_RECORD(env, 0 /*n_1*/)->cont;
                ENV_RECORD(env, 0 /*n_1*/)->cont = &&flip_n_1_lbl_36;
                ENV_RECORD(env, 0 /*n_1*/)->env = env;
                ENV_RECORD(env, 0 /*n_1*/)->index = 0;
                env = tmp_env;
                goto *tmp_cont;
flip_n_1_lbl_36:
              /* END flip(n_1):unfold-send */
              ENV_RECORD(env, 0 /*n_1*/)->write = ((unsigned char*)ENV_RECORD(env, 0 /*n_1*/) + sizeof(struct record));
              /* BEGIN select#S(n_1):88 */
                *(unsigned char*)ENV_RECORD(env, 0 /*n_1*/)->write = 0 /* #S */;
                ENV_RECORD(env, 0 /*n_1*/)->write += sizeof(unsigned char);
                /* BEGIN id(zero):88 */
                  tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                  ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
                  ENV_RECORD(env, 0 /*n_1*/)->polarity = 1;
                  env = tmp_env;
                  goto proc_zero;
                /* END id(zero):88 */
              /* END select#S(n_1):88 */
            /* END unfold-send(n_1):-1 */
          /* END select#S(n_1):88 */
        /* END unfold-send(n_1):-1 */
      /* END select#S(n_1):88 */
    /* END unfold-send(n_1):-1 */
cut_n_1_lbl_33:
    /* BEGIN cut(m_1):-1 */
      ENV_RECORD(env, 1 /*m_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
      ENV_RECORD(env, 1 /*m_1*/)->read = ENV_RECORD(env, 1 /*m_1*/)->write = ((unsigned char*)ENV_RECORD(env, 1 /*m_1*/) + sizeof(struct record));
      ENV_RECORD(env, 1 /*m_1*/)->cont = &&cut_m_1_lbl_37;
      ENV_RECORD(env, 1 /*m_1*/)->env = env;
      ENV_RECORD(env, 1 /*m_1*/)->index = 1;
      /* BEGIN unfold-send(m_1):-1 */
        /* BEGIN flip(m_1):unfold-send */
          tmp_env = ENV_RECORD(env, 1 /*m_1*/)->env;
          tmp_cont = ENV_RECORD(env, 1 /*m_1*/)->cont;
          ENV_RECORD(env, 1 /*m_1*/)->cont = &&flip_m_1_lbl_38;
          ENV_RECORD(env, 1 /*m_1*/)->env = env;
          ENV_RECORD(env, 1 /*m_1*/)->index = 1;
          env = tmp_env;
          goto *tmp_cont;
flip_m_1_lbl_38:
        /* END flip(m_1):unfold-send */
        ENV_RECORD(env, 1 /*m_1*/)->write = ((unsigned char*)ENV_RECORD(env, 1 /*m_1*/) + sizeof(struct record));
        /* BEGIN select#S(m_1):90 */
          *(unsigned char*)ENV_RECORD(env, 1 /*m_1*/)->write = 0 /* #S */;
          ENV_RECORD(env, 1 /*m_1*/)->write += sizeof(unsigned char);
          /* BEGIN unfold-send(m_1):-1 */
            /* BEGIN flip(m_1):unfold-send */
              tmp_env = ENV_RECORD(env, 1 /*m_1*/)->env;
              tmp_cont = ENV_RECORD(env, 1 /*m_1*/)->cont;
              ENV_RECORD(env, 1 /*m_1*/)->cont = &&flip_m_1_lbl_39;
              ENV_RECORD(env, 1 /*m_1*/)->env = env;
              ENV_RECORD(env, 1 /*m_1*/)->index = 1;
              env = tmp_env;
              goto *tmp_cont;
flip_m_1_lbl_39:
            /* END flip(m_1):unfold-send */
            ENV_RECORD(env, 1 /*m_1*/)->write = ((unsigned char*)ENV_RECORD(env, 1 /*m_1*/) + sizeof(struct record));
            /* BEGIN select#S(m_1):90 */
              *(unsigned char*)ENV_RECORD(env, 1 /*m_1*/)->write = 0 /* #S */;
              ENV_RECORD(env, 1 /*m_1*/)->write += sizeof(unsigned char);
              /* BEGIN unfold-send(m_1):-1 */
                /* BEGIN flip(m_1):unfold-send */
                  tmp_env = ENV_RECORD(env, 1 /*m_1*/)->env;
                  tmp_cont = ENV_RECORD(env, 1 /*m_1*/)->cont;
                  ENV_RECORD(env, 1 /*m_1*/)->cont = &&flip_m_1_lbl_40;
                  ENV_RECORD(env, 1 /*m_1*/)->env = env;
                  ENV_RECORD(env, 1 /*m_1*/)->index = 1;
                  env = tmp_env;
                  goto *tmp_cont;
flip_m_1_lbl_40:
                /* END flip(m_1):unfold-send */
                ENV_RECORD(env, 1 /*m_1*/)->write = ((unsigned char*)ENV_RECORD(env, 1 /*m_1*/) + sizeof(struct record));
                /* BEGIN select#S(m_1):90 */
                  *(unsigned char*)ENV_RECORD(env, 1 /*m_1*/)->write = 0 /* #S */;
                  ENV_RECORD(env, 1 /*m_1*/)->write += sizeof(unsigned char);
                  /* BEGIN id(zero):90 */
                    tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
                    ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 1 /*m_1*/);
                    ENV_RECORD(env, 1 /*m_1*/)->polarity = 1;
                    env = tmp_env;
                    goto proc_zero;
                  /* END id(zero):90 */
                /* END select#S(m_1):90 */
              /* END unfold-send(m_1):-1 */
            /* END select#S(m_1):90 */
          /* END unfold-send(m_1):-1 */
        /* END select#S(m_1):90 */
      /* END unfold-send(m_1):-1 */
cut_m_1_lbl_37:
      /* BEGIN cut(r_1):-1 */
        ENV_RECORD(env, 2 /*r_1*/) = malloc(sizeof(struct record) + (sizeof(unsigned char) + (/*#S*/ 0) + (/*#Z*/ 0)));
        ENV_RECORD(env, 2 /*r_1*/)->read = ENV_RECORD(env, 2 /*r_1*/)->write = ((unsigned char*)ENV_RECORD(env, 2 /*r_1*/) + sizeof(struct record));
        ENV_RECORD(env, 2 /*r_1*/)->cont = &&cut_r_1_lbl_41;
        ENV_RECORD(env, 2 /*r_1*/)->env = env;
        ENV_RECORD(env, 2 /*r_1*/)->index = 2;
        /* BEGIN id(mul):92 */
          tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (6) + sizeof(struct type_var) * (0));
          ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 0 /*n_1*/);
          ENV_RECORD(env, 0 /*n_1*/)->polarity = 0;
          ENV_RECORD(tmp_env, 1 /*m_1*/) = ENV_RECORD(env, 1 /*m_1*/);
          ENV_RECORD(env, 1 /*m_1*/)->polarity = 0;
          ENV_RECORD(tmp_env, 2 /*r_1*/) = ENV_RECORD(env, 2 /*r_1*/);
          ENV_RECORD(env, 2 /*r_1*/)->polarity = 1;
          env = tmp_env;
          goto proc_mul;
        /* END id(mul):92 */
cut_r_1_lbl_41:
        /* BEGIN id(printNat):94 */
          tmp_env = malloc(sizeof(struct environment) + sizeof(struct record*) * (1) + sizeof(struct type_var) * (0));
          ENV_RECORD(tmp_env, 0 /*n_1*/) = ENV_RECORD(env, 2 /*r_1*/);
          ENV_RECORD(env, 2 /*r_1*/)->polarity = 0;
          env = tmp_env;
          goto proc_printNat;
        /* END id(printNat):94 */
      /* END cut(r_1):-1 */
    /* END cut(m_1):-1 */
  /* END cut(n_1):-1 */

run:
  env = malloc(sizeof(struct environment) + sizeof(struct record*) * (3) + sizeof(struct type_var) * (0));
  goto proc_main;
end:
  return 0;
}

