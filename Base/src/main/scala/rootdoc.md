A library for writing [[https://docs.scala-lang.org/scala3/book/string-interpolation.html#advanced-usage custom string interpolation]] implementations via parser combinators

## Entry Points

If the string context should create an object:
Create leaf parsers using the methods in [[name.rayrobdod.stringContextParserCombinator.Interpolator$ the Interpolator companion object]],
combine and manipulate them with the methods in [[name.rayrobdod.stringContextParserCombinator.Interpolator! Interpolator]],
then interpolate using the final `Interpolator`'s `interpolate` method.

If the string context should create an extractor:
Create leaf parsers using the methods in [[name.rayrobdod.stringContextParserCombinator.Extractor$ the Extractor companion object]],
combine and manipulate them with the methods in [[name.rayrobdod.stringContextParserCombinator.Extractor! Extractor]],
then interpolate using the final `Extractor`'s `extractor` method.

If the string context should do both:
Create leaf parsers using the methods in [[name.rayrobdod.stringContextParserCombinator.Parser$ the Parser companion object]],
combine and manipulate them with the methods in [[name.rayrobdod.stringContextParserCombinator.Parser! Parser]],
then interpolate using the final `Parser`'s `interpolate` and `extractor` methods.
