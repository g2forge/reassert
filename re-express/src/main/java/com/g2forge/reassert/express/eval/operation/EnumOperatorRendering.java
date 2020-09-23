package com.g2forge.reassert.express.eval.operation;

import com.g2forge.reassert.express.model.operation.IOperation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class EnumOperatorRendering<Operator extends Enum<Operator> & IOperation.IOperator> implements IOperatorRendering {
	protected final Operator operator;

	protected final String pastVerb;

	@Override
	public String getName() {
		return getOperator().name().toLowerCase();
	}
}
