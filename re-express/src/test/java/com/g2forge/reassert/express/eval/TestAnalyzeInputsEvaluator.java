package com.g2forge.reassert.express.eval;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.AnalyzeInputsEvaluator;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.environment.Environment;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.variable.Closure;
import com.g2forge.reassert.express.model.variable.Variable;

public class TestAnalyzeInputsEvaluator {
	@Test
	public void literal() {
		HAssert.assertEquals(HCollection.asSet(), new AnalyzeInputsEvaluator<String, Boolean>().eval(new Literal<>("x", false)));
	}

	@Test
	public void operation() {
		final Variable<String, Boolean> x = new Variable<>("x"), y = new Variable<>("y");;
		HAssert.assertEquals(HCollection.asSet(x, y), new AnalyzeInputsEvaluator<String, Boolean>().eval(BooleanOperation.and(x, y)));
	}

	@Test
	public void variable() {
		final Variable<String, Boolean> x = new Variable<>("x");
		HAssert.assertEquals(HCollection.asSet(x), new AnalyzeInputsEvaluator<String, Boolean>().eval(x));
	}

	@Test
	public void closure() {
		final Variable<String, Boolean> x = new Variable<>("x"), z = new Variable<>("z");
		HAssert.assertEquals(HCollection.asSet(z), new AnalyzeInputsEvaluator<String, Boolean>().eval(new Closure<>(Environment.<String, Boolean>builder().bind$(x, false).bind$("y", true).build(), BooleanOperation.and(x, z))));
	}
}
