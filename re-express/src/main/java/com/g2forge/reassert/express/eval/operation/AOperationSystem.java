package com.g2forge.reassert.express.eval.operation;

import java.util.EnumMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.IOperation.IOperator;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class AOperationSystem<Value, Operator extends Enum<Operator> & IOperation.IOperator> implements IOperationSystem<Value> {
	protected final Class<Operator> operatorType;

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Map<Operator, IOperatorRendering> renderings = computeRenderings();

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Map<Operator, IOperatorDescriptor<Value>> descriptors = computeDescriptors();

	protected abstract IOperatorDescriptor<Value> computeDescriptor(Operator operator);

	protected Map<Operator, IOperatorDescriptor<Value>> computeDescriptors() {
		return Stream.of(getOperatorType().getEnumConstants()).collect(Collectors.toMap(IFunction1.identity(), this::computeDescriptor, HCollector.mergeFail(), () -> new EnumMap<>(getOperatorType())));
	}

	protected abstract IOperatorRendering computeRendering(Operator operator);

	protected Map<Operator, IOperatorRendering> computeRenderings() {
		return Stream.of(getOperatorType().getEnumConstants()).collect(Collectors.toMap(IFunction1.identity(), this::computeRendering, HCollector.mergeFail(), () -> new EnumMap<>(getOperatorType())));
	}

	@Override
	public IOperatorDescriptor<Value> getDescriptor(IOperator operator) {
		return getDescriptors().get(getOperator(operator));
	}

	protected Operator getOperator(IOperator operator) {
		final Class<Operator> operatorType = getOperatorType();
		if (!operatorType.isInstance(operator)) throw new UnsupportedOperationException(String.format("%1$s only supports %2$s operations!", getClass().getSimpleName(), operatorType.getName()));
		return operatorType.cast(operator);
	}

	@Override
	public IOperatorRendering getRendering(IOperator operator) {
		return getRenderings().get(getOperator(operator));
	}
}
