:-include("../data/data_sbrinz1.txt").

%% asq: If we know the data store size, what should we do?
%%      Two options:
%%         1.: Create NUMANode-sized partitions and place multiple partitions
%%             on same nodes
%%         2.: Ignore and just return the same number of partitions as there are NUMA nodes

% get_optimal_number_of_partitions(DataStoreSize, MaxParallel, NrPartitions, PartitionsList) :-
get_optimal_number_of_partitions(NrPartitions, PartitionsList) :-
    findall(X, memory_affinity(_, _, X), Affinities),
    sort(Affinities,AffinityList),
    ( foreach(AffinityDomain, AffinityList),
      foreach(Region, PartitionsList)
      do
        findall(L, memory_affinity(L, _, AffinityDomain), AfL),
        findall(H, (memory_affinity(L1, S, AffinityDomain), H is L1 + S), AfH),
        findall(Sz, memory_affinity(_, Sz, AffinityDomain), Sizes),
        eclipse_language:min(AfL, RL),
        eclipse_language:max(AfH, RH),
        sum(Sizes, MaxSize),
        findall(Core, cpu_affinity(Core, _, AffinityDomain), Cores),
        Region = partition(RL, RH, MaxSize, cores(Cores))
    ),
    length(PartitionsList, NrPartitions).



get_optimal_resource_allocation(NrPartitions, independent, Partitions) :-
    findall(X, memory_affinity(_, _, X), Affinities),
    sort(Affinities,AffinityList),
    length(AffinityList, NrNodes),
    NrMulF is NrPartitions / NrNodes,
    ceiling(NrMulF, NrMul2),
    integer(NrMul2, NrMul),
    
    Nr is NrMul - 1,
    ( for(_,0,Nr),
      foreach(A, AffinityLists),
      param(AffinityList)
      do
        A=AffinityList
    ),
    flatten(AffinityLists, PartitionsList),
    append(RelevantAffinities, _, PartitionsList),
    length(RelevantAffinities, NrPartitions),
    ( foreach(AffinityDomain, RelevantAffinities),
      foreach(Region, Partitions)
      do
        findall(L, memory_affinity(L, _, AffinityDomain), AfL),
        findall(H, (memory_affinity(L1, S, AffinityDomain), H is L1 + S), AfH),
        findall(Sz, memory_affinity(_, Sz, AffinityDomain), Sizes),
        eclipse_language:min(AfL, RL),
        eclipse_language:max(AfH, RH),
        sum(Sizes, MaxSize),
        findall(Core, cpu_affinity(Core, _, AffinityDomain), Cores),
        Region = partition(RL, RH, MaxSize, cores(Cores))
    ).    







multisplice([[]|_],[]).
multisplice([H|T],L) :-
    [H2|T2] = H,
    append(T, [T2], TN),
    multisplice(TN,M),
    append([H2],M,L).


get_optimal_resource_allocation_falsch(NrPartitions, PartitionSize, MaxParallel, independent, Partitions) :-
    findall(X, memory_affinity(_, _, X), Affinities),
    sort(Affinities,AffinityList),
    length(AffinityList, NrNodes),
    findall(_, cpu_affinity(_, _, _), Tmp),
    length(Tmp, NrCores),
    eclipse_language:min([NrPartitions, NrCores], MaxNrPartitions),
    ( foreach(A, AffinityList),
      foreach(CoreList, CoreLists)
      do
        findall(Core, cpu_affinity(Core, _, A), CoreList)
    ),
    multisplice(CoreLists, SplicedCoreList),
    append(Partitions, _, SplicedCoreList),
    length(Partitions, MaxNrPartitions).


get_optimal_resource_allocation_falsch(NrPartitions, PartitionSize, MaxParallel, collaborating, Partitions) :-
    findall(X, memory_affinity(_, _, X), Affinities),
    sort(Affinities,AffinityList),
    length(AffinityList, NrNodes),
    findall(_, cpu_affinity(_, _, _), Tmp),
    length(Tmp, NrCores),
    eclipse_language:min([NrPartitions, NrCores], MaxNrPartitions),
    ( foreach(A, AffinityList),
      foreach(CoreList, CoreLists)
      do
        findall(Core, cpu_affinity(Core, _, A), CoreList)
    ),
    flatten(CoreLists, CoreListF),
    append(Partitions, _, CoreListF),
    length(Partitions, MaxNrPartitions).
    
