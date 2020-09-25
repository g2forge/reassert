package com.g2forge.reassert.express.convert;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.value.ObjectValueSystem;
import com.g2forge.reassert.express.model.constant.Literal;

public class TestExplainLiteral {
	@Test
	public void named() {
		final String actual = new ExplanationRenderer<String, Integer>(ExplanationMode.Explain, ObjectValueSystem.create(), null).render(new Literal<>("myname", 3));
		HAssert.assertEquals("3 - myname", actual);
	}

	@Test
	public void unnamed() {
		final String actual = new ExplanationRenderer<String, Boolean>(ExplanationMode.Explain, ObjectValueSystem.create(), null).render(new Literal<>(false));
		HAssert.assertEquals("false", actual);
	}
}
