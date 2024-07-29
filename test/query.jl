using Test
using ACSets
using DiagrammaticEquations

# Prevent output order invariance
function check_queryoutput(query, expected)
    sort(query) == sort(expected)
end

SchTestBasicQueryACSet = BasicSchema([:src,:tgt], [(:f,:src,:tgt)])
@acset_type TestBasicQueryACSet(SchTestBasicQueryACSet, index=[:f])

SchTestDeepQueryACSet = BasicSchema([:Lvl1, :Lvl2, :Lvl3], [(:Map1,:Lvl1, :Lvl2), (:Map2,:Lvl2, :Lvl3)])
@acset_type TestDeepQueryACSet(SchTestDeepQueryACSet, index=[:Map1, :Map2])

SchTestMultiTableQueryACSet = BasicSchema([:x, :y, :a, :b], [(:f,:x,:y), (:g,:a,:b)])
@acset_type TestMultiTableQueryACSet(SchTestMultiTableQueryACSet, index=[:f, :g])

SchTestDecGraphQueryACSet = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)], [:X], [(:dec,:E,:X)])
@acset_type TestDecGraphQueryACSet(SchTestDecGraphQueryACSet, index=[:src,:tgt])

@testset "Basic single queries" begin
    singlesrctgt_example = @acset TestBasicQueryACSet begin
        src = 1
        tgt = 1
        f = [1]
    end
    
    @test check_queryoutput(collected_incident(singlesrctgt_example, 1, [:f]), [1])
    @test check_queryoutput(collected_incident(singlesrctgt_example, 1, [:f], [:f]), [1])

    doublesrctgt_example = @acset TestBasicQueryACSet begin
        src = 2
        tgt = 2
        f = [2, 1]
    end

    @test check_queryoutput(collected_incident(doublesrctgt_example, 1, [:f]), [2])
    @test check_queryoutput(collected_incident(doublesrctgt_example, 1, [:f], [:f]), [1])

    @test check_queryoutput(collected_incident(doublesrctgt_example, 2, [:f]), [1])
    @test check_queryoutput(collected_incident(doublesrctgt_example, 2, [:f], [:f]), [2])

    noresult_example = @acset TestBasicQueryACSet begin
        src = 1
        tgt = 2
        f = [1]
    end

    @test isempty(collected_incident(noresult_example, 2, [:f]))
    @test isempty(collected_incident(noresult_example, 2, [:f], [:f]))

    multipleresult_example = @acset TestBasicQueryACSet begin
        src = 3
        tgt = 2
        f = [1, 2, 2]
    end

    @test check_queryoutput(collected_incident(multipleresult_example, 2, [:f]), [2, 3])
    @test check_queryoutput(collected_incident(multipleresult_example, 2, [:f], [:f]), [2])

    # Check that using arrays does not affect query
    @test check_queryoutput(collected_incident(multipleresult_example, [2], [:f]), [2, 3])
    @test check_queryoutput(collected_incident(multipleresult_example, [2], [:f], [:f]), [2])
end

@testset "Deep single queries" begin
    singlepath_example = @acset TestDeepQueryACSet begin
        Lvl1=1
        Lvl2=1
        Lvl3=1
        Map1=[1]
        Map2=[1]
    end

    @test check_queryoutput(collected_incident(singlepath_example, 1, [[:Map1, :Map2]]), [1])
    @test check_queryoutput(collected_incident(singlepath_example, 1, [[:Map1, :Map2]], [:Map1]), [1])

    manysinglepaths_example = @acset TestDeepQueryACSet begin
        Lvl1=2
        Lvl2=2
        Lvl3=2
        Map1=[2,1]
        Map2=[2,1]
    end

    @test check_queryoutput(collected_incident(manysinglepaths_example, 2, [[:Map1, :Map2]]), [2])
    @test check_queryoutput(collected_incident(manysinglepaths_example, 2, [[:Map1, :Map2]], [:Map1]), [1])

    manyresults_firstquery_example = @acset TestDeepQueryACSet begin
        Lvl1=3
        Lvl2=3
        Lvl3=2
        Map1=[3, 2, 1]
        Map2=[1, 2, 2]
    end

    @test check_queryoutput(collected_incident(manyresults_firstquery_example, 2, [[:Map1, :Map2]]), [1, 2])
    @test check_queryoutput(collected_incident(manyresults_firstquery_example, 2, [[:Map1, :Map2]], [:Map1]), [3, 2])

    manyresults_lastquery_example = @acset TestDeepQueryACSet begin
        Lvl1=3
        Lvl2=2
        Lvl3=3
        Map1=[1, 2, 2]
        Map2=[1, 2]
    end

    @test check_queryoutput(collected_incident(manyresults_lastquery_example, 2, [[:Map1, :Map2]]), [2,3])
    @test check_queryoutput(collected_incident(manyresults_lastquery_example, 2, [[:Map1, :Map2]], [:Map1]), [2])

    manyresults_allqueries_example = @acset TestDeepQueryACSet begin
        Lvl1=3
        Lvl2=3
        Lvl3=2
        Map1=[1, 2, 2]
        Map2=[2, 2, 1]
    end

    @test check_queryoutput(collected_incident(manyresults_allqueries_example, 2, [[:Map1, :Map2]]), [1,2,3])
end

@testset "Multi-Table queries" begin
    presentinboth_example = @acset TestMultiTableQueryACSet begin
        x=1
        y=1
        a=1
        b=1
        f=[1]
        g=[1]
    end

    @test check_queryoutput(collected_incident(presentinboth_example, 1, [:f, :g]), [1])

    multires_example = @acset TestMultiTableQueryACSet begin
        x=3
        y=2
        a=2
        b=1
        f=[1,1,2]
        g=[1,1]
    end
    @test check_queryoutput(collected_incident(multires_example, 1, [:f, :g]), [1,2])
    # Check that querying works if no results in one table
    @test check_queryoutput(collected_incident(multires_example, 2, [:f, :g]), [3])

end

@testset "Combined queries" begin
    doublesrctgt_example = @acset TestBasicQueryACSet begin
        src = 2
        tgt = 2
        f = [2, 1]
    end

    @test check_queryoutput(collected_incident(doublesrctgt_example, [1, 2], [:f]), [2, 1])
    @test check_queryoutput(collected_incident(doublesrctgt_example, [1, 2], [:f], [:f]), [1, 2])

    # Check that input order does not affect queries
    @test check_queryoutput(collected_incident(doublesrctgt_example, [2, 1], [:f]), [2, 1])
    @test check_queryoutput(collected_incident(doublesrctgt_example, [2, 1], [:f], [:f]), [1, 2])

    manyresults_deep_example = @acset TestDeepQueryACSet begin
        Lvl1=3
        Lvl2=3
        Lvl3=3
        Map1=[2, 1, 3]
        Map2=[3, 2, 1]
    end

    @test check_queryoutput(collected_incident(manyresults_deep_example, [1, 2], [[:Map1, :Map2]]), [1, 3])

    stargraph_example = @acset TestDecGraphQueryACSet{Symbol} begin
        V = 4
        E = 3
        src = [2,3,4]
        tgt = [1,1,1]
        dec = [:a, :b, :c]
    end
    names_ofedges_tgtcenter = collected_incident(stargraph_example, 1, [:tgt], [:dec])
    @test check_queryoutput(names_ofedges_tgtcenter, [:a, :b, :c])
    names_ofedges_srcrest = collected_incident(stargraph_example, [2,3,4], [:src], [:dec])
    @test check_queryoutput(names_ofedges_srcrest, names_ofedges_tgtcenter)

    #Collect vertices of edge named :a
    @test check_queryoutput(collected_incident(stargraph_example, :a, [:dec, :dec], [:src, :tgt]), [1,2])

    clustergraph_example = @acset TestDecGraphQueryACSet{Symbol} begin
      V = 6
      E = 7
      src = [1,2,3,4,5,6,1]
      tgt = [2,3,1,5,6,4,4]
      dec = [:a, :b, :c, :d, :e, :f, :g]
    end

    names_ofedges_src1 = collected_incident(clustergraph_example, 1, [:src], [:dec])
    @test check_queryoutput(names_ofedges_src1, [:a, :g])
    names_ofedges_withvertex1 = collected_incident(clustergraph_example, 1, [:src, :tgt], [:dec, :dec])
    @test check_queryoutput(names_ofedges_withvertex1, [:a, :c, :g])

    tgts_with_src1 = collected_incident(clustergraph_example, 1, [:src], [:tgt])
    @test check_queryoutput(collected_incident(clustergraph_example, tgts_with_src1, [:src], [:tgt]), [3, 5])

    # Get vertex 4 neighbors
    neighbors_of4 = collected_incident(clustergraph_example, 4, [:src, :tgt], [:tgt, :src])
    @test check_queryoutput(neighbors_of4, [1,5,6])
    
    treegraph_exaxmple = @acset TestDecGraphQueryACSet{Symbol} begin
      V = 5
      E = 4
      src = [1,2,3,5]
      tgt = [2,3,4,4]
      dec = [:a, :b, :c, :d]
    end

    distance2_from4 = [4]
    distance1_from4 = collected_incident(treegraph_exaxmple, 4, [:tgt], [:src])
    @test check_queryoutput(distance1_from4, [3,5])
    distance2_from4 = collected_incident(treegraph_exaxmple, distance1_from4, [:tgt], [:src])
    @test check_queryoutput(distance2_from4, [2])
end