buildCApplication{ 
    target = "pagetable",
    cFiles = { "pagetable.c" },
    addLibraries = {"posixcompat"}
}


buildCApplication{ 
    target = "pagefaults",
    cFiles = { "protect.c" },
    addLibraries = {"posixcompat"}
}

