package com.g2forge.reassert.express.eval.bool;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.ATestExplainingEvaluator;
import com.g2forge.reassert.express.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.operation.BooleanOperationSystem;
import com.g2forge.reassert.express.eval.value.ObjectValueSystem;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExplained.Relevance;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.ImpliesExplainedOperation;

public class TestBooleanExplainingEvaluator extends ATestExplainingEvaluator<Boolean> {
	@Override
	protected IEvaluator<String, Boolean, IExplained<Boolean>> computeEvaluator() {
		return new ExplainingEvaluator<>(ObjectValueSystem.create(), BooleanOperationSystem.create());
	}

	public IOperation.IOperator getMultiply() {
		return BooleanOperation.Operator.AND;
	}

	@Override
	public Boolean getOne() {
		return true;
	}

	@Override
	public Boolean getZero() {
		return false;
	}

	@Test
	public void implies() {
		final IExplained<Boolean> expected = ImpliesExplainedOperation.<Boolean>builder().premise$(Relevance.Dominant, false).conclusion$(Relevance.Unevaluated, true).value(true).build();
		final IExplained<Boolean> actual = getEvaluator().eval(BooleanOperation.implies(new Literal<>(false), new Literal<>(true)));
		HAssert.assertEquals(expected, actual);
	}
}
