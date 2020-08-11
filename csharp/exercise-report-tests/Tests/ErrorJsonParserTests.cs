using System;
using System.Collections.Generic;
using Xunit;
using static ExerciseReport.Tests.Utils;

namespace ExerciseReport.Tests
{
    public class ErrorJsonParserTests
    {
        List<Error> errors = new List<Error>
        {
            new Error(ErrorSource.Design, Severity.Error, "Some design text" ),
            new Error(ErrorSource.Exercise, Severity.Fatal, "Some exercise text 2" ),
            new Error(ErrorSource.Merge, Severity.Error, "Some merge text 2" )
        };

        [Fact]
        public void Serialize_ErrorsList_ProducesWellFormedJson()
        {
            var erh = new ExerciseResourceHandler();
            var efc = new ExerciseReader(
                erh, new ExerciseJsonParser());
            efc.WriteExercises(Result.FatalError,
                new ExerciseObjectTree{Exercises = new List<Exercise>()},
                errors);
            Assert.Equal(GetResourceAsString(Constants.ErrorsSimpleResource), erh.ErrorResultJson);
        }

        [Fact]
        public void Parse_WellFormedErrorsJson_ProducesValidErrorList()
        {
            var errorsJson = GetResourceAsString(Constants.ErrorsSimpleResource);
        
            var ejp = new ExerciseJsonParser();
            Assert.Equal(errors, ejp.ErrorsFromString(errorsJson).Errors,
                new ErrorComparer());
        }
    }

    internal class ErrorComparer : IEqualityComparer<Error>
    {
        public bool Equals(Error x, Error y)
        {
            return x.Message == y.Message
                   && x.Severity == y.Severity
                   && x.Source == y.Source;
        }

        public int GetHashCode(Error err)
        {
            return HashCode.Combine(err.Message, err.Severity, err.Source);
        }
    }
}