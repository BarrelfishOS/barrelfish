##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

try:
    from mercurial import hg, ui, node, error, commands
    mercurial_module = True
except ImportError:
    mercurial_module = False

try:
    import git
    git_module = True
except ImportError:
    git_module = False

class Checkout(object):
    '''Checkout Base class:
    Maintain information about revision number, and directory locations
    '''

    def __init__(self, base_dir):
        # Set parameters
        self.base_dir = base_dir

    def get_base_dir(self):
        return self.base_dir

    ''' Returns a unique commit identifier as string '''
    def get_revision(self):
        return '(repository information/git/hg module not available)'

    ''' Return a string (or None) containing a patch file representing local changes'''
    def get_diff(self):
        return None

    ''' Return a dict of additional info '''
    def get_meta(self):
        return {}


class CheckoutHg(Checkout):
    def __init__(self, base_dir, repo):
       super(CheckoutHg, self).__init__(base_dir) 
       self.repo = repo

    def get_revision(self):
       # identify the parents of the working revision
       context = self.repo[None]
       parents = context.parents()
       s = ', '.join(map(lambda p: node.short(p.node()), parents))
       if context.files() or context.deleted():
           s += ' with local changes'
       else:
           s += ' unmodified'
       return s


    def get_diff(self):
       context = self.repo[None]
       if not context.files() and not context.deleted():
           return None

       diffui = ui.ui()
       diffui.pushbuffer()
       commands.diff(diffui, self.repo, git=True)
       return diffui.popbuffer()

class CheckoutGit(Checkout):
    def __init__(self, base_dir, repo):
       super(CheckoutGit, self).__init__(base_dir) 
       self.repo = repo

    def get_diff(self):
        gitc = self.repo.git
        return gitc.diff() + "\n\n" + gitc.diff(cached=True)

    def get_revision(self):
        return self.repo.head.commit.hexsha

    def get_meta(self):
        repo = self.repo
        headc = repo.head.commit
        ret = {}
        shortsha = self.repo.git.rev_parse(headc.hexsha, short=7)
        try:
            ret["branch"] = repo.active_branch.name
        except TypeError, e:
            ret["branch"] = "(HEAD detached at %s)" % shortsha
        ret["shortrev"] = shortsha
        ret["commitmsg"] = headc.message.split("\n")[0]
        ret["commitmsg-tail"] = "".join(headc.message.split("\n")[1:])
        ret["dirty"] = str(self.repo.is_dirty())
        ret["untracked-files"] = ",".join(self.repo.untracked_files)
        return ret
        


''' Factory method that checks a directory and instantiates
the correct Checkout class '''
def create_for_dir(base_dir):
    if git_module:
        try:
            return CheckoutGit(base_dir, git.Repo(base_dir))
        except git.InvalidGitRepositoryError:
            pass

    if mercurial_module:
        try:
            return CheckoutHg(base_dir, hg.repository(ui.ui(), base_dir))
        except error.RepoError:
            pass

    return Checkout(base_dir)



