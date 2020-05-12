namespace Nabu

module internal Interop  =
    let inline flip f x1 x2 = f x2 x1

    let inline flip2 f x1 x2 x3 = f x3 x2 x1

    let inline curry f x1 x2 = f(x1, x2)

    let inline curry2 f x1 x2 x3 = f(x1, x2, x3)

open System.Collections.Concurrent

module internal Memorize =
    let inline memorize (f: 'key -> 'value) =
        let cache = ConcurrentDictionary<'key, 'value> ()
        fun key -> cache.GetOrAdd(key, f)