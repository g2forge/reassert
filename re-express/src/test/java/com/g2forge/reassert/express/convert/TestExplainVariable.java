package com.g2forge.reassert.express.convert;

import org.junit.Test;

import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.convert.ExplanationMode;
import com.g2forge.reassert.express.convert.ExplanationRenderer;
import com.g2forge.reassert.express.eval.integer.IntegerValueSystem;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.variable.ExplainedVariable;
import com.g2forge.reassert.express.model.variable.Variable;

public class TestExplainVariable {
	@Test
	public void novalue() {
		final String actual = new ExplanationRenderer<String, Integer>(ExplanationMode.Explain, IntegerValueSystem.create(), null).render(new ExplainedVariable<>(new Variable<String, Integer>("var"), NullableOptional.empty()));
		HAssert.assertEquals("var", actual);
	}

	@Test
	public void value() {
		final String actual = new ExplanationRenderer<String, Integer>(ExplanationMode.Explain, IntegerValueSystem.create(), null).render(new ExplainedVariable<>(new Variable<String, Integer>("var"), NullableOptional.of(new Literal<>("literal", -4))));
		HAssert.assertEquals("var = -4 - literal", actual);
	}
}
