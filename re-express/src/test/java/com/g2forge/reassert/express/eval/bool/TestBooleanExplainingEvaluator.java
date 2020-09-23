package com.g2forge.reassert.express.eval.bool;

import com.g2forge.reassert.express.eval.ATestExplainingEvaluator;
import com.g2forge.reassert.express.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.bool.BooleanOperationSystem;
import com.g2forge.reassert.express.eval.bool.BooleanValueSystem;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;

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
