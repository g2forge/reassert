package com.g2forge.reassert.express.v2.eval.operation;

import com.g2forge.reassert.express.v2.model.operation.IOperation;

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
