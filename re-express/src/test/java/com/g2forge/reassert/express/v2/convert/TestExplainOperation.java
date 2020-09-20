package com.g2forge.reassert.express.v2.convert;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.integer.IntegerOperationSystem;
import com.g2forge.reassert.express.v2.eval.integer.IntegerValueSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.IExplained.Relevance;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.v2.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.v2.model.operation.ZeroExplainedOperation;

public class TestExplainOperation extends ATestExplanationRenderer<String, Integer> {
	@Override
	protected IOperationSystem<Integer> getOperationSystem() {
		return IntegerOperationSystem.create();
	}

	@Override
	protected IValueSystem<? super Integer> getValueSystem() {
		return IntegerValueSystem.create();
	}

	@Test
	public void identity() {
		HAssert.assertEquals("1 (*) - because the * operator (with identity 1) was applied", render(ExplanationMode.Explain, new ExplainedOperation<>(1, "*", 1)));
		HAssert.assertEquals("1 (*) - because there's a single argument: 1", render(ExplanationMode.Explain, new ExplainedOperation<Integer>(1, "*", 1).toBuilder().argument$(true, new Literal<>(1)).build()));
		HAssert.assertEquals("2 (*) - because the * operator (with identity 1) was applied\n\t* 2 - Non-identity", render(ExplanationMode.Explain, new ExplainedOperation<Integer>(2, "*", 1).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("Non-identity", 2)).build()));
		HAssert.assertEquals("2 (*) - because the * operator (with identity 1) was applied\n\t_ 1\n\t* 2 - Non-identity", render(ExplanationMode.Trace, new ExplainedOperation<Integer>(2, "*", 1).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("Non-identity", 2)).build()));
		HAssert.assertEquals("2 (*) - because there's a single relevant argument: 1", render(ExplanationMode.Describe, new ExplainedOperation<Integer>(2, "*", 1).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("Non-identity", 2)).build()));
	}

	@Test
	public void identityNoArgs() {
		HAssert.assertEquals("0 - because 0 is the identity for multiplication", render(ExplanationMode.Explain, ExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).value(0).identity$(0).build()));
	}

	@Test
	public void zero() {
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator", render(ExplanationMode.Explain, new ZeroExplainedOperation<Integer>("*", 0)));
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator\n\t* 0", render(ExplanationMode.Explain, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(true, new Literal<>(0)).build()));
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator\n\t* 0 - ZERO!", render(ExplanationMode.Explain, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("ZERO!", 0)).build()));
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator\n\t_ 1\n\t* 0 - ZERO!", render(ExplanationMode.Trace, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("ZERO!", 0)).build()));
		HAssert.assertEquals("0 (*) - because one or more arguments were 0, for example: 0 - ZERO!", render(ExplanationMode.Describe, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("ZERO!", 0)).build()));
	}

	@Test
	public void zero0() {
		HAssert.assertEquals("0 - because anything multiplied by 0 is 0\n\t> 0", render(ExplanationMode.Explain, ZeroExplainedOperation.<Integer>builder().operator(ArithmeticOperation.Operator.MULTIPLY).zero(0).argument$(Relevance.Dominant, 0).build()));
	}
}
