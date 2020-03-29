Practical 1: Factoring Numbers

Here is a simple method for finding the smallest prime factor of a positive
integer:

> factor :: Integer -> (Integer, Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>    where (q,r) = n `divMod` m

for example

*Main> factor 7654321
(19,402859)

because 

*Main> 19 * 402859
7654321

Repeatedly extracting the smallest factor will return a list
of prime factors:

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>    where (p,q) = factorFrom m n

for example

*Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]


Exercise 1
          factor 0 = factorFrom 2 0 = (2,0), as 0 = 2*0 + 0 and so (q,r) = (0,0) => r = 0, hence
                     r == 0 and so the first guard is true.
          factor 1 = factorFrom 2 1 which will run infinitely, as 1 = 2*0 + 1 and so (q,r) = (0,1) 
                     => r /= 0 and so the second guard is true, i.e. factorFrom 3 1 is evaluated,
                     but 1 = 3*0 + 1 and so again (q,r) = (0,1) => r /= 0 and so the second guard
                     is true again, evaluating factorFrom 4 1 and so on, evaluating forever.


Exercise 2
          factor 0 output: (2,0)
          factor 1 output: infinite loop


Exercise 3
          If n is prime, the smallest prime factor of n is n and n is not less than n.
          Otherwise, suppose that the smallest factor of n, called p >= 2, is bigger than sqrt(n)
          and less than n. Then there is q > 1 such that n = p * q > sqrt(n) * q => q < sqrt(n).
          But sqrt(n) < p and so q < p, contradicting the minimality of p. 
          Hence the smallest factor of n must be less than or equal to sqrt(n).

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n

> factorFrom1 m n | r == 0    = (m,q) 
>                 | n <= m*m  = (n,1)
>                 | otherwise = factorFrom1 (m+1) n
>                 where (q,r) = n `divMod` m

         The order of the guards matters. Say we had written n <= m*m first. If n = p*p and we
         come to evaluate factorFrom1 p n, we would get (n,1), since the first true guard is
         n <= m*m, even if the second guard r == 0 is true as well. This way, we miss the factor p 

         The worst case is the one when n is prime, because when divided by any 2 <= m < p, the 
         remainder r is never equal to 0, hence the first guard is never true. If m is in the 
         range n > m*m, the third guard is true and we get one recursive call here. But n > m*m 
         is true for m <= floor (sqrt n), equality possible because n is prime and hence not a
         perfect square.
         Then the number of recursive calls which are needed in the worst case is: sqrt n - 2 + 1
         i.e. sqrt n - 1


Exercise 4
          If n <= m*m is true, we either have n == m*m is true or n < m*m is true. For the first
          one, note that we also have r == 0, and so the first guard is true, i.e. the second one
          is ignored. If n < m*m, then mq <= n < m*m and so q < m.

          If q < m is true, then mq < m*m is also true. But n = mq + r and this means that
          n < m*m + r. If r == 0, we get that n = mq and so factorFrom2 q n should evaluate
          as (q,m) since its first guard r == 0 is 0. Hence we would never get to factorFrom2 m n
          which is absurd. This means that r > 0 and from n < m*m + r we get that n <= m*m.

          Informally:
          As q = n 'div' m we get that mq <= n < mq + m. We want n > m*m and so we want 
          mq + m > n > m*m and so we want m(q+1) > m*m and so we want q+1 > m and so we want 
          q >= m. Then what we do not want is q < m.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 m n | r == 0    = (m,q)
>                 | q < m     = (n,1)  
>                 | otherwise = factorFrom2 (m+1) n
>                 where (q,r) = n `divMod` m

          This version is more efficient because, at each step, we save one operation: m*m, which
          is both time and memory consuming.

Exercise 5
          
> factor3 :: Integer -> (Integer, Integer)
> factor3 n = factorFrom3 2 n

> factorFrom3 m n | r == 0    = (m,q)
>                 | q < m     = (n,1)
>                 | m == 2    = factorFrom3 (m+1) n 
>                 | otherwise = factorFrom3 (m+2) n
>                 where (q,r) = n `divMod` m

          Since now we ignore every even number greater than 2, I expect this version to be twice
          more efficient than the previous one. This is because inside an interval [a,b], even
          numbers take up half the space.

Exercise 6
          Some results:
          factor3 16         --- (2,8)         --- (0.00 secs, 68,616 bytes)
          factor3 2017       --- (2017,1)      --- (0.00 secs, 86,808 bytes)
          factor3 8616460799 --- (89681,96079) --- (0.15 secs, 32,717,640 bytes)

Exercise 7

> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 2 n 2

> factorFrom4 m n s | r == 0    = (m,q)
>                   | q < m     = (n,1)
>                   | m == 2    = factorFrom4 (m+s-1) n s
>                   | m == 3    = factorFrom4 (m+s) n s
>                   | otherwise = factorFrom4 (m+s) n (6-s)
>                   where (q,r) = n `divMod` m

          Some results:
          factor4 16         --- (2,8)         --- (0.00 secs, 68,632 bytes)
          factor4 2017       --- (2017,1)      --- (0.00 secs, 83,624 bytes)
          factor4 8616460799 --- (89681,96079) --- (0.12 secs, 24,467,400 bytes)

Exercise 8
          A potential problem would be the fact that we do not have a formula for the nth prime
          number. This means that if we want to only consider primes as trial divisors, we will
          need a list of these primes, which must be constructed separately.

Exercise 9

> factors2 :: Integer -> [Integer]
> factors2 n = factorsFrom2 2 n

> factorsFrom2 m n | n == 1    = []
>                  | otherwise = p : factorsFrom2 p q
>                  where (p,q) | m `mod` 6 == 1 = factorFrom4 m n 4
>                              | otherwise      = factorFrom4 m n 2 

          factorsFrom2 2 n works by finding the smallest prime p >= 2 such that n = p * q.
          This is being done by factorFrom4 2 n 2. Once p is found, we add it to the result list
          and look for the factors of q. If there was p' < p such that p' | q, then p' | n and
          so p' would have been found instead of p, hence the factors of q are no less than p.
          This explains p : factorsFrom2 p q.

          factorsFrom2 p q finds the factors of q which are no less than p. We need the guard
          p `mod` 6 == 1 because p + 2 is a multiple of 3. Since 3 < p, from above we know that
          3 cannot be a factor of q, hence p+2 cannot be either. Hence, in factorFrom4 p q s,
          we must check the chain p, p+4, p+6, p+10 etc which requires s = 4.
          If p `mod` 6 == 5, we must check p+2, but not p+4 as it is a multiple of 3. Hence
          we must check the chain p, p+2, p+6, p+8 etc which requires s = 2.
          The only possibilities left are p == 2 or p == 3, which work with factorFrom p q 2 
          because they have special cases in factorFrom4.

          Hence we find the smallest factor of q and, by recursion, the factorisation of n follows.

Exercise 10
     Some comparsions:
             x --- factors x time --- factors2 x time --- factors x memory --- factors2 x memory
       1754643     0.06 secs          0.00 secs           19,212,232 bytes     139,728 bytes
17172606372407     0.09 secs          0.07 secs           34,664,048 bytes     25,428,216 bytes
    4991104801     0.08 secs          0.06 secs           25,658,096 bytes     19,922,456 bytes

     Finding the two factors of Jevon's number took factor2 0.07 secs and factor 0.09 secs.
     The two factors are 89681,96079.

Exercise 11
           If r < 0, it is pointless to reduce q, as y >= q and so this case was already checked.
           We should replace p by (p+1), as this is the only way to increase r.
           If r > 0, it is pointless to reduce p, as  x >= p and so this case was already checked. 
           We should replace q by (q+1), as this is the only way to decrease r.

           This method is guaranteed to terminate for all odd n because eventually it will reach
           x = (n+1)/2 and y = (n-1)/2, which satisfy x*x - y*y = n.

> search :: Integer -> Integer -> Integer -> (Integer, Integer)
> search p q n | r == 0    = (p,q) 
>              | r < 0     = search (p+1) q n
>              | otherwise = search p (q+1) n
>              where r  = p*p - q*q - n

           Note that this takes care of the requirement 0 <= y < x. If at some point p = q + 1,
           then search p (q+1) is never executed because this would mean that r > 0, i.e.
           p*p - (p-1)*(p-1) > n i.e. 2p-1 > n i.e. p > (n+1)/2 i.e. x > (n+1)/2 and y > (n-1)/2
           and so v = x + y > n = u * v, absurd.


> fermat :: Integer -> (Integer, Integer)
> fermat n = (u,v)
>            where (x,y) = search m 0 n;
>                      u = x - y;
>                      v = x + y;
>                      m = ceiling (sqrt (fromInteger n))

> isqrt :: Integer -> Integer 
> isqrt = truncate . sqrt . fromInteger 

Exercise 12
           The smallest possible value for x is ceiling (sqrt n) , as x*x = y*y + n >= n i.e.
           x >= sqrt n and since x is an integer, x >= ceiling (sqrt n).

Exercise 13
           fermat 8616460799 = (89681,96079) and fermat 1963272347809 = (241679,8123471)

Exercise 14

> search2 :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
> search2 p q n r | r == 0    = (p,q) 
>                 | r < 0     = search2 (p+1) q n (r + 2*p + 1)
>                 | otherwise = search2 p (q+1) n (r - 2*q - 1)

> fermat2 :: Integer -> (Integer, Integer)
> fermat2 n = (u,v)
>            where (x,y) = search2 m 0 n (m*m - n);
>                   u    = x - y;
>                   v    = x + y;
>                   m    = ceiling (sqrt (fromInteger n))

Exercise 15

> isqrt' :: Integer -> Integer
> isqrt' n = head [k | k <- [0..], (k+1)^2 > n]        


Exercise 16

> lsplit :: (Integer, Integer) -> (Integer, Integer)
> lsplit (l,r) = (l,m)
>              where m = (l+r) `div` 2

> rsplit :: (Integer, Integer) -> (Integer, Integer)
> rsplit (l,r) = (m,r)
>              where m = (l+r) `div` 2

Exercise 17

> isqrt1 :: Integer -> (Integer, Integer) -> Integer
> isqrt1 n (l,r) | l+1 == r  = l
>                | m^2 <= n  = isqrt1 n (rsplit (l,r))
>                | otherwise = isqrt1 n (lsplit (l,r))
>                where m = (l+r) `div` 2

           At each step we are dividing intervals in half. We start from (1,n) of lentgh n,
           then go to an interval of length n/2, then to an interval of length (n/2)/2 and
           so on until we get an inverval of length 2. If k is the number of steps, then
           n/2^k = 2 and so n = 2^(k+1) => k ~ log2 n - 1.

Exercise 18

> bound :: Integer -> Integer -> Integer
> bound n k | k * k >= n = k
>           | otherwise  = bound n (2*k)  

> isqrt2 n = isqrt1 n (1, bound n 1)

           For this implementation, the starting interval is (1, bound n 1) and
           bound n 1 = floor (sqrt n) + 1 and so, according to Exrcise 17, the number of 
           steps is k ~ log2 (floor (sqrt n) + 1) ~ log2 (sqrt n) ~ 1/2 * log2 n.
           The improvement is not impressive.

