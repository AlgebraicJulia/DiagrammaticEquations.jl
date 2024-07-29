using Test
using ACSets
using DiagrammaticEquations

# Prevent output order invariance
function check_queryoutput(query, expected)
    sort(query) == sort(expected)
end

SchTestBasicQueryACSet = BasicSchema([:src,:tgt], [(:f,:src,:tgt)])
@acset_type TestBasicQueryACSet(SchTestBasicQueryACSet, index=[:f])

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

SchTestDeepQueryACSet = BasicSchema([:Lvl1, :Lvl2, :Lvl3],
                                    [(:Map1,:Lvl1, :Lvl2), (:Map2,:Lvl2, :Lvl3)])
@acset_type TestDeepQueryACSet(SchTestDeepQueryACSet, index=[:Map1, :Map2])

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

SchTestMultiTableQueryACSet = BasicSchema([:x, :y, :a, :b], [(:f,:x,:y), (:g,:a,:b)])
@acset_type TestMultiTableQueryACSet(SchTestMultiTableQueryACSet, index=[:f, :g])

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
end