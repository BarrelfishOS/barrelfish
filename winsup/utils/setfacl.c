/* setfacl.c

   Copyright 2000, 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011, 2014
   Red Hat Inc.

   Written by Corinna Vinschen <vinschen@redhat.com>

This file is part of Cygwin.

This software is a copyrighted work licensed under the terms of the
Cygwin license.  Please consult the file "CYGWIN_LICENSE" for
details. */

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/acl.h>
#include <cygwin/version.h>

#ifndef BOOL
#define BOOL int
#endif

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

#ifndef ILLEGAL_MODE
#define ILLEGAL_MODE ((mode_t)0xffffffff)
#endif

static char *prog_name;

typedef enum {
  NoAction,
  Set,
  Modify,
  Delete,
  ModNDel,
  DeleteAll,
  DeleteDef,
  SetFromFile
} action_t;

mode_t getperm (char *in)
{
  if (isdigit ((unsigned char) *in) && !in[1])
    {
      int i = atoi (in);
      if (i < 0 || i > 7)
	return ILLEGAL_MODE;
      return i << 6 | i << 3 | i;
    }
  if (strlen (in) > 3 && strchr (" \t\n\r#", in[3]))
    in[3] = '\0';
  if (strlen (in) != 3)
    return ILLEGAL_MODE;
  if (!strchr ("r-", in[0])
      || !strchr ("w-", in[1])
      || !strchr ("x-", in[2]))
    return ILLEGAL_MODE;
  return (in[0] == 'r' ? S_IROTH : 0)
	 | (in[1] == 'w' ? S_IWOTH : 0)
	 | (in[2] == 'x' ? S_IXOTH : 0);
}

BOOL
getaclentry (action_t action, char *c, aclent_t *ace)
{
  char *c2;

  ace->a_type = 0;
  ace->a_id = (uid_t) -1;
  ace->a_perm = 0;

  /* First, check if we're handling a default entry. */
  if (!strncmp (c, "default:", 8) || !strncmp (c, "d:", 2))
    {
      ace->a_type = ACL_DEFAULT;
      c = strchr (c, ':') + 1;
    }
  /* c now points to the type.  Check for next colon.  If we find a colon,
     NUL it.  Otherwise the string is invalid, except when deleting. */
  c2 = strchrnul (c, ':');
  if (*c2 == ':')
    *c2++ = '\0';
  else if (action != Delete)
    return FALSE;
  /* Fetch the type. */
  if (!strcmp (c, "u") || !strcmp (c, "user"))
      ace->a_type |= USER_OBJ;
  else if (!strcmp (c, "g") || !strcmp (c, "group"))
      ace->a_type |= GROUP_OBJ;
  else if (!strcmp (c, "m") || !strcmp (c, "mask"))
      ace->a_type |= CLASS_OBJ;
  else if (!strcmp (c, "o") || !strcmp (c, "other"))
      ace->a_type |= OTHER_OBJ;
  else
    return FALSE;
  /* Skip to next field. */
  c = c2;
  if (!*c)
    {
      /* Nothing follows.  This is only valid if action is Delete and the
	 type is CLASS_OBJ, or if ACL_DEFAULT is set. */
      if (action != Delete
	  || (!(ace->a_type & (CLASS_OBJ | ACL_DEFAULT))))
	return FALSE;
    }
  else if (!(ace->a_type & (USER_OBJ | GROUP_OBJ)))
    {
      /* Mask and other entries may contain an extra colon. */
      if (*c == ':')
	++c;
    }
  /* If this is a user or group entry, check if next char is a colon char.
     If so, skip it, otherwise it's the name of a user or group. */
  else if (*c == ':')
    ++c;
  else if (*c)
    {
      /* c now points to the id.  Check for next colon.  If we find a colon,
	 NUL it.  Otherwise the string is invalid, except when deleting.
	 If we delete, it must be a default entry since standard ugo entries
	 can't be deleted. */
      c2 = strchrnul (c + 1, ':');
      if (*c2 == ':')
	*c2++ = '\0';
      else if (action != Delete)
	return FALSE;
      /* Fetch user/group id. */
      if (isdigit ((unsigned char) *c))
	{
	  char *c3;

	  ace->a_id = strtol (c, &c3, 10);
	  if (*c3)
	    return FALSE;
	}
      else if (ace->a_type & USER_OBJ)
	{
	  struct passwd *pw = getpwnam (c);
	  if (!pw)
	    return FALSE;
	  ace->a_id = pw->pw_uid;
	}
      else
	{
	  struct group *gr = getgrnam (c);
	  if (!gr)
	    return FALSE;
	  ace->a_id = gr->gr_gid;
	}
      if (ace->a_type & USER_OBJ)
	{
	  ace->a_type &= ~USER_OBJ;
	  ace->a_type |= USER;
	}
      else
	{
	  ace->a_type &= ~GROUP_OBJ;
	  ace->a_type |= GROUP;
	}
      /* Skip to next field. */
      c = c2;
    }
  if (action == Delete)
    {
      /* Trailing garbage? */
      if (*c)
	return FALSE;
      /* No, we're good. */
      ace->a_perm = ILLEGAL_MODE;
      return TRUE;
    }
  /* Check perms. */
  if ((ace->a_perm = getperm (c)) == ILLEGAL_MODE)
    return FALSE;
  return TRUE;
}

BOOL
getaclentries (action_t action, char *buf, aclent_t *acls, int *idx)
{
  char *c;

  if (action == SetFromFile)
    {
      FILE *fp;
      char fbuf[256], *fb;

      if (!strcmp (buf, "-"))
	fp = stdin;
      else if (! (fp = fopen (buf, "r")))
	return FALSE;
      while ((fb = fgets (fbuf, 256, fp)))
	{
	  while (strchr (" \t", *fb))
	    ++fb;
	  if (strchr ("\n\r#", *fb))
	    continue;
	  if (!getaclentry (action, fb, acls + (*idx)++))
	    {
	      fclose (fp);
	      return FALSE;
	    }
	}
      if (fp != stdin)
	fclose (fp);
    }
  else
    for (c = strtok (buf, ","); c; c = strtok (NULL, ","))
      if (!getaclentry (action, c, acls + (*idx)++))
	return FALSE;
  return TRUE;
}

int
searchace (aclent_t *aclp, int nentries, int type, int id)
{
  int i;

  for (i = 0; i < nentries; ++i)
    if ((aclp[i].a_type == type && (id < 0 || aclp[i].a_id == id))
	|| !aclp[i].a_type)
      return i;
  return -1;
}

int
modacl (aclent_t *tgt, int tcnt, aclent_t *src, int scnt)
{
  int t, s, i;

  for (s = 0; s < scnt; ++s)
    {
      t = searchace (tgt, MAX_ACL_ENTRIES, src[s].a_type,
		     (src[s].a_type & (USER | GROUP)) ? src[s].a_id : -1);
      if (t < 0)
	return -1;
      if (src[s].a_perm == ILLEGAL_MODE)
	{
	  if (t < tcnt)
	    {
	      for (i = t + 1; i < tcnt; ++i)
		tgt[i - 1] = tgt[i];
	      --tcnt;
	    }
	}
      else
	{
	  tgt[t] = src[s];
	  if (t >= tcnt)
	    ++tcnt;
	}
    }
  return tcnt;
}

int
addmissing (aclent_t *tgt, int tcnt)
{
  int t;
  int types = 0, def_types = 0;
  int perm = 0, def_perm = 0;

  /* Check if we have all the required entries now. */
  for (t = 0; t < tcnt; ++t)
    if (tgt[t].a_type & ACL_DEFAULT)
      {
	def_types |= tgt[t].a_type;
	if (tgt[t].a_type & (USER | GROUP | GROUP_OBJ))
	  def_perm |= tgt[t].a_perm;
      }
    else
      {
	types |= tgt[t].a_type;
	if (tgt[t].a_type & (USER | GROUP | GROUP_OBJ))
	  perm |= tgt[t].a_perm;
      }
  /* Add missing CLASS_OBJ */
  if ((types & (USER | GROUP)) && !(types & CLASS_OBJ))
    {
      tgt[tcnt].a_type = CLASS_OBJ;
      tgt[tcnt].a_id = (uid_t) -1;
      tgt[tcnt++].a_perm = perm;
    }
  if (def_types)
    {
      /* Add missing default entries. */
      if (!(def_types & USER_OBJ) && tcnt < MAX_ACL_ENTRIES)
	{
	  t = searchace (tgt, tcnt, USER_OBJ, -1);
	  tgt[tcnt].a_type = DEF_USER_OBJ;
	  tgt[tcnt].a_id = (uid_t) -1;
	  tgt[tcnt++].a_perm = t >= 0 ? tgt[t].a_perm : S_IRWXO;
	}
      if (!(def_types & GROUP_OBJ) && tcnt < MAX_ACL_ENTRIES)
	{
	  t = searchace (tgt, tcnt, GROUP_OBJ, -1);
	  tgt[tcnt].a_type = DEF_GROUP_OBJ;
	  tgt[tcnt].a_id = (uid_t) -1;
	  tgt[tcnt].a_perm = t >= 0 ? tgt[t].a_perm : (S_IROTH | S_IXOTH);
	  def_perm |= tgt[tcnt++].a_perm;
	}
      if (!(def_types & OTHER_OBJ) && tcnt < MAX_ACL_ENTRIES)
	{
	  t = searchace (tgt, tcnt, OTHER_OBJ, -1);
	  tgt[tcnt].a_type = DEF_OTHER_OBJ;
	  tgt[tcnt].a_id = (uid_t) -1;
	  tgt[tcnt++].a_perm = t >= 0 ? tgt[t].a_perm : (S_IROTH | S_IXOTH);
	}
      /* Add missing DEF_CLASS_OBJ */
      if ((def_types & (USER | GROUP)) && !(def_types & CLASS_OBJ))
	{
	  tgt[tcnt].a_type = DEF_CLASS_OBJ;
	  tgt[tcnt].a_id = (uid_t) -1;
	  tgt[tcnt++].a_perm = def_perm;
	}
    }
  return tcnt;
}

int
delacl (aclent_t *tgt, int tcnt, action_t action)
{
  int t;

  for (t = 0; t < tcnt; ++t)
    /* -b (DeleteAll): Remove all extended ACL entries.
       -k (DeleteDef): Remove all default ACL entries. */
    if ((action == DeleteAll && (tgt[t].a_type & (USER | GROUP | CLASS_OBJ)))
	|| (action == DeleteDef && (tgt[t].a_type & ACL_DEFAULT)))
      {
	--tcnt;
	if (t < tcnt)
	  memmove (&tgt[t], &tgt[t + 1], (tcnt - t) * sizeof (aclent_t));
	--t;
      }
  return tcnt;
}

int
setfacl (action_t action, const char *path, aclent_t *acls, int cnt)
{
  aclent_t lacl[MAX_ACL_ENTRIES];
  int lcnt;

  memset (lacl, 0, sizeof lacl);
  switch (action)
    {
    case Set:
      if (acl (path, SETACL, cnt, acls))
	{
	  perror (prog_name);
	  return 2;
	}
      break;
    case Delete:
      if ((lcnt = acl (path, GETACL, MAX_ACL_ENTRIES, lacl)) < 0
	  || (lcnt = modacl (lacl, lcnt, acls, cnt)) < 0
	  || (lcnt = acl (path, SETACL, lcnt, lacl)) < 0)
	{
	  perror (prog_name);
	  return 2;
	}
      break;
    case DeleteAll:
    case DeleteDef:
      if ((lcnt = acl (path, GETACL, MAX_ACL_ENTRIES, lacl)) < 0
	  || (lcnt = delacl (lacl, lcnt, action)) < 0
	  || (lcnt = acl (path, SETACL, lcnt, lacl)) < 0)
	{
	  perror (prog_name);
	  return 2;
	}
      break;
    default:
      if ((lcnt = acl (path, GETACL, MAX_ACL_ENTRIES, lacl)) < 0
	  || (lcnt = modacl (lacl, lcnt, acls, cnt)) < 0
	  || (lcnt = addmissing (lacl, lcnt)) < 0
	  || (lcnt = acl (path, SETACL, lcnt, lacl)) < 0)
	{
	  perror (prog_name);
	  return 2;
	}
      break;
    }
  return 0;
}

static void
usage (FILE *stream)
{
  fprintf (stream, ""
	    "Usage: %s [-r] {-f ACL_FILE | -s acl_entries} FILE...\n"
	    "       %s [-r] {-b|[-d acl_entries] [-m acl_entries]} FILE...\n"
	    "\n"
	    "Modify file and directory access control lists (ACLs)\n"
	    "\n"
	    "  -b, --remove-all remove all extended ACL entries\n"
	    "  -d, --delete     delete one or more specified ACL entries\n"
	    "  -f, --file       set ACL entries for FILE to ACL entries read\n"
	    "                   from a ACL_FILE\n"
	    "  -k, --remove-default\n"
	    "                   remove all default ACL entries\n"
	    "  -m, --modify     modify one or more specified ACL entries\n"
	    "  -r, --replace    replace mask entry with maximum permissions\n"
	    "                   needed for the file group class\n"
	    "  -s, --substitute substitute specified ACL entries for the\n"
	    "                   ACL of FILE\n"
	    "  -h, --help       output usage information and exit\n"
	    "  -V, --version    output version information and exit\n"
	    "\n"
	    "At least one of (-b, -d, -f, -k, -m, -s) must be specified\n"
	    "\n", prog_name, prog_name);
  if (stream == stdout)
  {
    printf(""
	    "     Acl_entries are one or more comma-separated ACL entries \n"
	    "     from the following list:\n"
	    "\n"
	    "         u[ser]::perm\n"
	    "         u[ser]:uid:perm\n"
	    "         g[roup]::perm\n"
	    "         g[roup]:gid:perm\n"
	    "         m[ask]:perm\n"
	    "         o[ther]:perm\n"
	    "\n"
	    "     Default entries are like the above with the additional\n"
	    "     default identifier. For example: \n"
	    "\n"
	    "         d[efault]:u[ser]:uid:perm\n"
	    "\n"
	    "     'perm' is either a 3-char permissions string in the form\n"
	    "     \"rwx\" with the character - for no permission\n"
	    "     or it is the octal representation of the permissions, a\n"
	    "     value from 0 (equivalent to \"---\") to 7 (\"rwx\").\n"
	    "     'uid' is a user name or a numerical uid.\n"
	    "     'gid' is a group name or a numerical gid.\n"
	    "\n"
	    "\n"
	    "For each file given as parameter, %s will either replace its\n"
	    "complete ACL (-s, -f), or it will add, modify, or delete ACL\n"
	    "entries.\n"
	    "\n"
	    "The following options are supported:\n"
	    "\n"
	    "-b   Remove all extended ACL entries.  The base ACL entries of the\n"
	    "     owner, group and others are retained.\n"
	    "\n"
	    "-d   Delete one or more specified entries from the file's ACL.\n"
	    "     The owner, group and others entries must not be deleted.\n"
	    "     Acl_entries to be deleted should be specified without\n"
	    "     permissions, as in the following list:\n"
	    "\n"
	    "         u[ser]:uid[:]\n"
	    "         g[roup]:gid[:]\n"
	    "         m[ask][:]\n"
	    "         d[efault]:u[ser][:uid]\n"
	    "         d[efault]:g[roup][:gid]\n"
	    "         d[efault]:m[ask][:]\n"
	    "         d[efault]:o[ther][:]\n"
	    "\n"
	    "-f   Take the Acl_entries from ACL_FILE one per line. Whitespace\n"
	    "     characters are ignored, and the character \"#\" may be used\n"
	    "     to start a comment.  The special filename \"-\" indicates\n"
	    "     reading from stdin.\n"
	    "     Required entries are\n"
	    "     - One user entry for the owner of the file.\n"
	    "     - One group entry for the group of the file.\n"
	    "     - One other entry.\n"
	    "     If additional user and group entries are given:\n"
	    "     - A mask entry for the file group class of the file.\n"
	    "     - No duplicate user or group entries with the same uid/gid.\n"
	    "     If it is a directory:\n"
	    "     - One default user entry for the owner of the file.\n"
	    "     - One default group entry for the group of the file.\n"
	    "     - One default mask entry for the file group class.\n"
	    "     - One default other entry.\n"
	    "\n"
	    "-k   Remove all default ACL entries. If no default ACL entries\n"
	    "     exist, no warnings are issued.\n"
	    "\n"
	    "-m   Add or modify one or more specified ACL entries.\n"
	    "     Acl_entries is a comma-separated list of entries from the \n"
	    "     same list as above.\n"
	    "\n"
	    "-r   Causes the permissions specified in the mask entry to be\n"
	    "     ignored and replaced by the maximum permissions needed for\n"
	    "     the file group class.\n"
	    "\n"
	    "-s   Like -f, but substitute the file's ACL with Acl_entries\n"
	    "     specified in a comma-separated list on the command line.\n"
	    "\n"
	    "While the -d and -m options may be used in the same command, the\n"
	    "-f and -s options may be used only exclusively.\n"
	    "\n"
	    "Directories may contain default ACL entries.  Files created\n"
	    "in a directory that contains default ACL entries will have\n"
	    "permissions according to the combination of the current umask,\n"
	    "the explicit permissions requested and the default ACL entries\n"
	    "\n", prog_name);
  }
  else
    fprintf(stream, "Try '%s --help' for more information.\n", prog_name);
}

struct option longopts[] = {
  {"remove-all", no_argument, NULL, 'b'},
  {"delete", required_argument, NULL, 'd'},
  {"file", required_argument, NULL, 'f'},
  {"remove-default", no_argument, NULL, 'k'},
  {"modify", required_argument, NULL, 'm'},
  {"replace", no_argument, NULL, 'r'},
  {"substitute", required_argument, NULL, 's'},
  {"help", no_argument, NULL, 'h'},
  {"version", no_argument, NULL, 'V'},
  {0, no_argument, NULL, 0}
};
const char *opts = "bd:f:hkm:rs:V";

static void
print_version ()
{
  printf ("setfacl (cygwin) %d.%d.%d\n"
	  "POSIX ACL modification utility\n"
	  "Copyright (C) 2000 - %s Red Hat, Inc.\n"
	  "This is free software; see the source for copying conditions.  There is NO\n"
	  "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n",
	  CYGWIN_VERSION_DLL_MAJOR / 1000,
	  CYGWIN_VERSION_DLL_MAJOR % 1000,
	  CYGWIN_VERSION_DLL_MINOR,
	  strrchr (__DATE__, ' ') + 1);
}

int
main (int argc, char **argv)
{
  int c;
  action_t action = NoAction;
  int ropt = 0;
  aclent_t acls[MAX_ACL_ENTRIES];
  int aclidx = 0;
  int ret = 0;

  prog_name = program_invocation_short_name;

  memset (acls, 0, sizeof acls);
  while ((c = getopt_long (argc, argv, opts, longopts, NULL)) != EOF)
    switch (c)
      {
      case 'b':
	if (action == NoAction)
	  action = DeleteAll;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	break;
      case 'd':
	if (action == NoAction)
	  action = Delete;
	else if (action == Modify)
	  action = ModNDel;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	if (! getaclentries (Delete, optarg, acls, &aclidx))
	  {
	    fprintf (stderr, "%s: illegal acl entries\n", prog_name);
	    return 2;
	  }
	break;
      case 'f':
	if (action == NoAction)
	  action = Set;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	if (! getaclentries (SetFromFile, optarg, acls, &aclidx))
	  {
	    fprintf (stderr, "%s: illegal acl entries\n", prog_name);
	    return 2;
	  }
	break;
      case 'h':
	usage (stdout);
	return 0;
      case 'k':
	if (action == NoAction)
	  action = DeleteDef;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	break;
      case 'm':
	if (action == NoAction)
	  action = Modify;
	else if (action == Delete)
	  action = ModNDel;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	if (! getaclentries (Modify, optarg, acls, &aclidx))
	  {
	    fprintf (stderr, "%s: illegal acl entries\n", prog_name);
	    return 2;
	  }
	break;
      case 'r':
	if (!ropt)
	  ropt = 1;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	break;
      case 's':
	if (action == NoAction)
	  action = Set;
	else
	  {
	    usage (stderr);
	    return 1;
	  }
	if (! getaclentries (Set, optarg, acls, &aclidx))
	  {
	    fprintf (stderr, "%s: illegal acl entries\n", prog_name);
	    return 2;
	  }
	break;
      case 'V':
	print_version ();
	return 0;
      default:
	fprintf (stderr, "Try `%s --help' for more information.\n", prog_name);
	return 1;
      }
  if (action == NoAction)
    {
      usage (stderr);
      return 1;
    }
  if (optind > argc - 1)
    {
      usage (stderr);
      return 1;
    }
  if (action == Set)
    switch (aclcheck (acls, aclidx, NULL))
      {
      case GRP_ERROR:
	fprintf (stderr, "%s: more than one group entry.\n", prog_name);
	return 2;
      case USER_ERROR:
	fprintf (stderr, "%s: more than one user entry.\n", prog_name);
	return 2;
      case CLASS_ERROR:
	fprintf (stderr, "%s: more than one mask entry.\n", prog_name);
	return 2;
      case OTHER_ERROR:
	fprintf (stderr, "%s: more than one other entry.\n", prog_name);
	return 2;
      case DUPLICATE_ERROR:
	fprintf (stderr, "%s: duplicate additional user or group.\n", prog_name);
	return 2;
      case ENTRY_ERROR:
	fprintf (stderr, "%s: invalid entry type.\n", prog_name);
	return 2;
      case MISS_ERROR:
	fprintf (stderr, "%s: missing entries.\n", prog_name);
	return 2;
      case MEM_ERROR:
	fprintf (stderr, "%s: out of memory.\n", prog_name);
	return 2;
      default:
	break;
      }
  for (c = optind; c < argc; ++c)
    ret |= setfacl (action, argv[c], acls, aclidx);
  return ret;
}
