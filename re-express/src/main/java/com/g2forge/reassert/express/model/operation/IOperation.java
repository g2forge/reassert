package com.g2forge.reassert.express.model.operation;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.builder.IValidatingBuilder;
import com.g2forge.alexandria.java.validate.CompositeValidation;
import com.g2forge.alexandria.java.validate.IValidatable;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.variable.Variable;

public interface IOperation<Name, Value> extends IExpression<Name, Value> {
	public interface IOperationBuilder<Name, Value, Builder extends IOperationBuilder<Name, Value, Builder, Built>, Built extends IOperation<Name, Value>> extends IValidatingBuilder<Built> {
		public Builder argument(IExpression<Name, Value> argument);

		public default Builder argument$L(Name name, Value value) {
			return argument(new Literal<>(name, value));
		}

		public default Builder argument$L(Value value) {
			return argument(new Literal<>(value));
		}

		public default Builder argument$V(Name name) {
			return argument(new Variable<>(name));
		}

		public Builder arguments(Collection<? extends IExpression<Name, Value>> arguments);
	}

	public interface IOperator {
		public <Name, Value> IOperationBuilder<Name, Value, ?, ?> builder();

		public default IValidation validate(List<? extends IExpression<?, ?>> arguments) {
			return new OperatorArgumentsValidation(true);
		}
	}

	public List<? extends IExpression<Name, Value>> getArguments();

	public IOperator getOperator();

	@Override
	public default IValidation validate() {
		final List<IValidation> arguments = getArguments().stream().map(IValidatable::validate).collect(Collectors.toList());
		return CompositeValidation.create(HCollection.concatenate(arguments, HCollection.asList(getOperator().validate(getArguments()))));
	}
}
