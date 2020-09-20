package com.g2forge.reassert.express.v2.eval.bool;

import com.g2forge.reassert.express.v2.eval.ATestExplainingEvaluator;
import com.g2forge.reassert.express.v2.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.v2.eval.IEvaluator;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

import lombok.Getter;

public class TestBooleanExplainingEvaluator extends ATestExplainingEvaluator<Boolean> {
	@Getter(lazy = true)
	private final IEvaluator<String, Boolean, IExplained<Boolean>> evaluator = new ExplainingEvaluator<>(BooleanValueSystem.create(), BooleanOperationSystem.create());

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
}
