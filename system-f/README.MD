# System-f

[system-f](https://github.com/system-f/fp-course)

## GHCi Utils

```
>> :info swiggletwoop
>> :i

>> :type reverse
>> :t

>> :reload
>> :r

>> :load
>> :l
```

## Testing

```
>> test test_List

>> :l src/Test/ListTest.hs
>> test headOrTest
>> test productTest

-- Custom cmd
>> :test test_List
>> :test headOrTest
```

## One-day

Sometimes this course material is condensed into one-day. In these cases, the
following exercises are recommended:

* `Optional`
  * `mapOptional`
  * `bindOptional`
  * `(??)`
  * `(<+>)`
* `List`
  * `headOr`
  * `product`
  * `length`
  * `map`
  * `filter`
  * `(++)`
  * `flatMap`
  * `reverse`
* `Functor`
  * `instance Functor List`
  * `instance Functor Optional`
  * `instance Functor ((->) t)`
  * `instance Functor void`
* `Applicative`
  * `instance Applicative List`
  * `instance Applicative Optional`
  * `instance Applicative ((->) t)`
  * `lift2`
  * `sequence`
* `FileIO`
