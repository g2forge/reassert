package com.g2forge.reassert.express.convert;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.operation.BooleanOperationSystem;
import com.g2forge.reassert.express.eval.operation.IntegerOperationSystem;
import com.g2forge.reassert.express.eval.value.ObjectValueSystem;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExplained.Relevance;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.model.operation.ImpliesExplainedOperation;
import com.g2forge.reassert.express.model.operation.ZeroExplainedOperation;

public class TestExplainOperation {
	@Test
	public void combined23() {
		final IExplained<Integer> explained = ExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).value(6).identity$(1).argument$(Relevance.Combined, 2).argument$(Relevance.Combined, 3).build();
		HAssert.assertEquals("6 - multiply\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("6 - multiply\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("6 - multiply\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("6 - multiply\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void identity1() {
		final IExplained<Integer> explained = ExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).value(1).identity$(1).argument$(Relevance.Identity, 1).build();
		HAssert.assertEquals("1 - because anything multiplied by 1 is itself, and the only argument is 1", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("1 - because anything multiplied by 1 is itself, and the only argument is 1", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("1 - because anything multiplied by 1 is itself, and the only argument is 1", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("1 - because anything multiplied by 1 is itself\n\t- 1", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void identity12() {
		final IExplained<Integer> explained = ExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).value(2).identity$(1).argument$(Relevance.Identity, 1).argument$(Relevance.Combined, 2).build();
		HAssert.assertEquals("2 - because anything multiplied by 1 is itself, and the relevant argument is 2", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("2 - because anything multiplied by 1 is itself, and the relevant argument is 2", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("2 - because anything multiplied by 1 is itself\n\t* 2", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("2 - because anything multiplied by 1 is itself\n\t- 1\n\t* 2", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void identity123() {
		final IExplained<Integer> explained = ExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).value(6).identity$(1).argument$(Relevance.Identity, 1).argument$(Relevance.Combined, 2).argument$(Relevance.Combined, 3).build();
		HAssert.assertEquals("6 - because anything multiplied by 1 is itself\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("6 - because anything multiplied by 1 is itself\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("6 - because anything multiplied by 1 is itself\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("6 - because anything multiplied by 1 is itself\n\t- 1\n\t* 2\n\t* 3", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void identityNoArgs() {
		final IExplained<Integer> explained = ExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).value(1).identity$(1).build();
		for (ExplanationMode mode : ExplanationMode.values()) {
			HAssert.assertEquals(mode.toString(), "1 - because anything multiplied by 1 is itself, and there are no arguments", renderInteger(mode, explained));
		}
	}

	@Test
	public void impliesFalseAny() {
		final IExplained<Boolean> explained = ImpliesExplainedOperation.<Boolean>builder().value(true).premise$(IExplained.Relevance.Dominant, false).conclusion$(IExplained.Relevance.Unevaluated, false).build();
		HAssert.assertEquals("true - because the premise is false", renderBoolean(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("true - because the premise is false", renderBoolean(ExplanationMode.Describe, explained));
		HAssert.assertEquals("true - because the premise is false\n\t> false", renderBoolean(ExplanationMode.Explain, explained));
		HAssert.assertEquals("true - because the premise is false\n\t> false\n\t  false", renderBoolean(ExplanationMode.Trace, explained));
	}

	@Test
	public void impliesTrueFalse() {
		final IExplained<Boolean> explained = ImpliesExplainedOperation.<Boolean>builder().value(true).premise$(IExplained.Relevance.Combined, true).conclusion$(IExplained.Relevance.Combined, false).build();
		HAssert.assertEquals("true - because the premise is true, and the conclusion is false", renderBoolean(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("true - because the premise is true, and the conclusion is false", renderBoolean(ExplanationMode.Describe, explained));
		HAssert.assertEquals("true - because the premise is true, and the conclusion is false\n\t* true\n\t* false", renderBoolean(ExplanationMode.Explain, explained));
		HAssert.assertEquals("true - because the premise is true, and the conclusion is false\n\t* true\n\t* false", renderBoolean(ExplanationMode.Trace, explained));
	}

	@Test
	public void impliesTrueTrue() {
		final IExplained<Boolean> explained = ImpliesExplainedOperation.<Boolean>builder().value(true).premise$(IExplained.Relevance.Combined, true).conclusion$(IExplained.Relevance.Combined, true).build();
		HAssert.assertEquals("true - because the premise is true, and the conclusion is true", renderBoolean(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("true - because the premise is true, and the conclusion is true", renderBoolean(ExplanationMode.Describe, explained));
		HAssert.assertEquals("true - because the premise is true, and the conclusion is true\n\t* true\n\t* true", renderBoolean(ExplanationMode.Explain, explained));
		HAssert.assertEquals("true - because the premise is true, and the conclusion is true\n\t* true\n\t* true", renderBoolean(ExplanationMode.Trace, explained));
	}

	@Test
	public void not() {
		final IExplained<Boolean> explained = ExplainedOperation.<Boolean>builder().operator(BooleanOperation.Operator.NOT).value(false).argument$(Relevance.Combined, true).build();
		HAssert.assertEquals("false - not, and the only argument is true", renderBoolean(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("false - not, and the only argument is true", renderBoolean(ExplanationMode.Describe, explained));
		HAssert.assertEquals("false - not, and the only argument is true", renderBoolean(ExplanationMode.Explain, explained));
		HAssert.assertEquals("false - not\n\t* true", renderBoolean(ExplanationMode.Trace, explained));
	}

	protected String renderBoolean(ExplanationMode mode, final IExplained<Boolean> explained) {
		return new ExplanationRenderer<>(mode, ObjectValueSystem.create(), BooleanOperationSystem.create()).render(explained);
	}

	protected String renderInteger(ExplanationMode mode, final IExplained<Integer> explained) {
		return new ExplanationRenderer<>(mode, ObjectValueSystem.create(), IntegerOperationSystem.create()).render(explained);
	}

	@Test
	public void zero0() {
		final IExplained<Integer> explained = ZeroExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).zero(0).argument$(Relevance.Dominant, 0).build();
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void zero01() {
		final IExplained<Integer> explained = ZeroExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).zero(0).argument$(Relevance.Dominant, 0).argument$(Relevance.Unevaluated, 1).build();
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0\n\t  1", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void zero02() {
		final IExplained<Integer> explained = ZeroExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).zero(0).argument$(Relevance.Dominant, 0).argument$(Relevance.Unevaluated, 2).build();
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0\n\t  2", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void zero10() {
		final IExplained<Integer> explained = ZeroExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).zero(0).argument$(Relevance.Identity, 1).argument$(Relevance.Dominant, 0).build();
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t- 1\n\t> 0", renderInteger(ExplanationMode.Trace, explained));
	}

	@Test
	public void zero20() {
		final IExplained<Integer> explained = ZeroExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).zero(0).argument$(Relevance.Combined, 2).argument$(Relevance.Dominant, 0).build();
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Summarize, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0, and one argument was 0", renderInteger(ExplanationMode.Describe, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", renderInteger(ExplanationMode.Explain, explained));
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t* 2\n\t> 0", renderInteger(ExplanationMode.Trace, explained));
	}
}
