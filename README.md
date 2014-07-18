SetDifferenceEstimator
======================

Task: Estimate the magnitude of the documents that need to be sychronized between servers.

Assuming we have two servers, with a bounded communication channel, containing a set of documents - we denote these sets with A resp. B. They are not necessarily disjoint, and we want to estimate the size of the difference, i.e. an approximation of

    m = |A \ B|+ |B \ A|
.

We have a set of independent hash functions g1,..,gn with range [0;1] and domain U. (That is the domain the sets A and B are in, i.e. A ⊆ U, B ⊆ U.) And a second set of hash functions: h1,..,hn: These have domain P(U), i.e. they can be applied to subsets of documents, instead of individual ones, and their range is a b-bit string, i.e.


    g_i : U → [0;1]
    h_i : P(U) → {0,..,2^b - 1}


for i ∈ {1,..,n}.

Instead of sending all documents, both server send only bn - bits, that is the hashes of

    x(i,A) := h( { x | x ∈ A, g_i(x) <= p_i } )
    x(i,B) := h( { x | x ∈ B, g_i(x) <= p_i } )

Note that we have choosen a sequence of probabilities p_1,..,p_n.

Consider the case, when the probability p_i is so small, that only a small portion of the documents in A (resp. B) are selected for hashing. (Assuming the set difference m is small, these sets will likely be equal, and hence the hash value.)

On the other hand if m would be big, then the probability, that selected set, will still be different, even if p_i is quite small is very high.

The probability of each event can be computed as:

    P(E_i) := P( x(i,A) /= x(i,B) ) =      (1-(1-p_i)^m)*(1-2^(-b))
    P(F_i) := P( x(i,A) == x(i,B) ) = 1 - [(1-(1-p_i)^m)*(1-2^(-b))]

The first probability is the product of the events that, there is at least one element from the set A\B or B\A in the sample, and the second event, is the probability that there is no hash collision.

Given a sequence of observations

     p_i, x(i,A), x(i,B)

can you estimate the parameter m (i.e. the count of distinct elements?).

To be precise, we are looking for the Maximum Likelihood Estimator of m, that is the m - best explaining the observed values for x(i,A) , x(i,B).

Note that: P(x(i,A)==x(i,B)) ist montone decresing with respect to increasing values of m, while P(x(i,A)/=x(i,B)) is monotone increasing. Note also that, we assumed the hashfunctions h_i, g_i are independent, hence the probability of intersections events for i ≠ j  will be products of the probabilities of the individual events, e.g.

    P(E_i and E_j) = P(E_i) * P(E_j) 
or

    P(E_i and F_j) = P(E_i) * P(F_j) 
and so on ...

    P(F_i and F_j) = P(F_i) * P(F_j) 
as long as i /= j.

(See also the source file Solution.hs)











