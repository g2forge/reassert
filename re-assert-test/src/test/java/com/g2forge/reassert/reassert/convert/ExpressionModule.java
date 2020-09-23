package com.g2forge.reassert.reassert.convert;

import org.slf4j.event.Level;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidatable;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.licenseusage.ICTName;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExpression;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ExpressionModule extends SimpleModule {
	protected static abstract class ContextualFindingMixin extends FindingMixin {
		@JsonIgnore
		public abstract Level getLevel();
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_EMPTY)
	protected static abstract class ExplainedMixin {}

	@JsonInclude(JsonInclude.Include.NON_EMPTY)
	protected static abstract class ExpressionContextualFindingMixin extends ContextualFindingMixin {
		@JsonIgnore
		protected IExpression<ICTName, TermRelation> expression;
	}

	@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY)
	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	protected static abstract class FindingMixin {
		@JsonIgnore
		public abstract IFinding getInnermostFinding();
	}

	protected static abstract class IOptionalMixin {
		@JsonIgnore
		public abstract boolean isNotEmpty();
	}

	protected static abstract class IValidatableMixin {
		@JsonIgnore
		public abstract boolean isValid();
	}

	private static final long serialVersionUID = -1153968277342886689L;

	protected final IFunction1<? super Object, ? extends IDescription> describer;

	@Override
	public void setupModule(SetupContext context) {
		setMixInAnnotation(IOptional.class, IOptionalMixin.class);
		setMixInAnnotation(IValidatable.class, IValidatableMixin.class);
		setMixInAnnotation(IFinding.class, FindingMixin.class);
		setMixInAnnotation(IContextFinding.class, ContextualFindingMixin.class);
		setMixInAnnotation(ExpressionContextFinding.class, ExpressionContextualFindingMixin.class);
		setMixInAnnotation(IExplained.class, ExplainedMixin.class);
		super.setupModule(context);
	}
}