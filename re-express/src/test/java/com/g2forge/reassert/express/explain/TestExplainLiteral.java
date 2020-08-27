package com.g2forge.reassert.express.explain;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.expression.explain.convert.ExplanationMode;
import com.g2forge.reassert.expression.express.Literal;

public class TestExplainLiteral extends ATestExplanationRenderer {
	@Test
	public void named() {
		HAssert.assertEquals("3 - myname", render(ExplanationMode.Explain, new Literal<>("myname", 3)));
	}

	@Test
	public void unnamed() {
		HAssert.assertEquals("false", render(ExplanationMode.Explain, new Literal<>(false)));
	}
}
