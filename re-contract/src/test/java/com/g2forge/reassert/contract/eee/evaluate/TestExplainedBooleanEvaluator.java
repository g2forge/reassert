package com.g2forge.reassert.contract.eee.evaluate;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.eee.evaluate.bool.BooleanBooleanSystem;
import com.g2forge.reassert.contract.eee.evaluate.bool.IBooleanSystem;
import com.g2forge.reassert.contract.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.contract.eee.express.Literal;
import com.g2forge.reassert.contract.eee.express.Operation;

public class TestExplainedBooleanEvaluator extends ATestExplainedBooleanEvaluator<Boolean> {
	@Test
	public void and() {
		final Operation<Boolean> expression = Operation.Operator.AND.<Boolean>builder().argument$("A", true).argument$("B", false).build();
		HAssert.assertEquals("false (&&) - because false is the zero of the && operator\n\t* false - B", explain(ExplanationMode.Explain, expression));
		HAssert.assertEquals("false (&&) - because false is the zero of the && operator\n\t_ true - A\n\t* false - B", explain(ExplanationMode.Trace, expression));
	}

	@Override
	protected IBooleanSystem<Boolean> createSystem() {
		return BooleanBooleanSystem.create();
	}

	@Test
	public void literal() {
		HAssert.assertEquals("false", explain(ExplanationMode.Trace, new Literal<>(false)));
		HAssert.assertEquals("true - input", explain(ExplanationMode.Trace, new Literal<>("input", true)));
	}

	@Test
	public void nested() {
		final Operation<Boolean> expression = Operation.Operator.OR.<Boolean>builder().argument(Operation.Operator.AND.<Boolean>builder().argument$("A", true).argument$("B", false).build()).argument$("C", true).build();
		HAssert.assertEquals("true (||) - because true is the zero of the || operator\n\t* true - C", explain(ExplanationMode.Explain, expression));
		HAssert.assertEquals("true (||) - because true is the zero of the || operator\n\t_ false (&&) - because false is the zero of the && operator\n\t\t_ true - A\n\t\t* false - B\n\t* true - C", explain(ExplanationMode.Trace, expression));
	}

	@Test
	public void not() {
		HAssert.assertEquals("false (!) - because there's a single argument: true - input", explain(ExplanationMode.Explain, Operation.Operator.NOT.<Boolean>builder().argument$("input", true).build()));
	}
}
