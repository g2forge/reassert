package com.g2forge.reassert.express.v2.convert;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.integer.IntegerValueSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.constant.Literal;

public class TestExplainLiteral extends ATestExplanationRenderer<String, Integer> {
	@Override
	protected IOperationSystem<Integer> getOperationSystem() {
		return null;
	}

	@Override
	protected IValueSystem<? super Integer> getValueSystem() {
		return IntegerValueSystem.create();
	}

	@Test
	public void named() {
		HAssert.assertEquals("3 - myname", render(ExplanationMode.Explain, new Literal<>("myname", 3)));
	}

	@Test
	public void unnamed() {
		HAssert.assertEquals("0", render(ExplanationMode.Explain, new Literal<>(0)));
	}
}
