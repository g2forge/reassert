package com.g2forge.reassert.express.v2.convert;

import org.junit.Test;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.integer.IntegerValueSystem;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.variable.ExplainedClosure;
import com.g2forge.reassert.express.v2.model.variable.ExplainedVariable;
import com.g2forge.reassert.express.v2.model.variable.IVariable;
import com.g2forge.reassert.express.v2.model.variable.Variable;

public class TestExplainClosure {
	@Test
	public void novariables() {
		final IExplained<Integer> explained = ExplainedClosure.<String, Integer>builder().expression(new Literal<>(0)).build();
		final String actual = new ExplanationRenderer<String, Integer>(ExplanationMode.Explain, IntegerValueSystem.create(), null).render(explained);
		HAssert.assertEquals("0 - result closure application, 0", actual);
	}

	@Test
	public void variables() {
		final IVariable<String, Integer> var = new Variable<String, Integer>("var");
		final IOptional<Literal<String, Integer>> optional = NullableOptional.of(new Literal<>("literal", -4));

		final IExplained<Integer> explained = ExplainedClosure.<String, Integer>builder().binding$(IExplained.Relevance.Combined, var, optional).expression(new ExplainedVariable<>(var, optional)).build();
		final String actual = new ExplanationRenderer<String, Integer>(ExplanationMode.Explain, IntegerValueSystem.create(), null).render(explained);
		HAssert.assertEquals("-4 - result closure application\n\t* var = -4 - literal\n\tvar = -4 - literal", actual);
	}
}
