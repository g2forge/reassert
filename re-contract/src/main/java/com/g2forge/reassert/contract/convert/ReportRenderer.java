package com.g2forge.reassert.contract.convert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.CopyrightNoticeFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.DiscloseSourceFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.StateChangesFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.SuspiciousUsageFinding;
import com.g2forge.reassert.contract.algorithm.worklicense.model.finding.IncompatibleWorkLicenseFinding;
import com.g2forge.reassert.contract.algorithm.worklicense.model.finding.UnknownWorkFinding;
import com.g2forge.reassert.contract.eval.TermRelationOperationSystem;
import com.g2forge.reassert.contract.eval.TermRelationValueSystem;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.finding.IConditionFinding;
import com.g2forge.reassert.contract.model.finding.IContractTermFinding;
import com.g2forge.reassert.contract.model.finding.UnrecognizedTermFinding;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.contract.ContractType;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.MultiLicenseFinding;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.MultiUsageFinding;
import com.g2forge.reassert.core.model.report.IContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.express.convert.ExplanationMode;
import com.g2forge.reassert.express.convert.ExplanationRenderer;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.ILiteral;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Getter;

@Getter
public class ReportRenderer extends ATextualRenderer<Object, IReportRenderContext> {
	@Getter(AccessLevel.PROTECTED)
	protected class ReportRenderContext extends ARenderContext implements IReportRenderContext {
		protected final ExplanationRenderer<IContractComparisonName, TermRelation> explanationRenderer;

		@Getter(AccessLevel.PUBLIC)
		protected ExpressionContextFinding findingContext;

		public ReportRenderContext(TextNestedModified.TextNestedModifiedBuilder builder, ExplanationRenderer<IContractComparisonName, TermRelation> explanationRenderer) {
			super(builder);
			this.explanationRenderer = explanationRenderer;
		}

		@Override
		public ICloseable findingContext(ExpressionContextFinding findingContext) {
			final ExpressionContextFinding previous = this.findingContext;
			this.findingContext = findingContext;
			return () -> {
				if (this.findingContext != findingContext) throw new IllegalStateException();
				this.findingContext = previous;
			};
		}

		@Override
		public ExplanationMode getMode() {
			return getExplanationRenderer().getMode();
		}

		@Override
		protected IReportRenderContext getThis() {
			return this;
		}

		@Override
		public IReportRenderContext name(IContractComparisonName name) {
			getExplanationRenderer().getNameRenderer().render(getBuilder(), name);
			return getThis();
		}

		@Override
		public IReportRenderContext render(IExplained<TermRelation> explained) {
			getExplanationRenderer().render(getBuilder(), explained);
			return getThis();
		}
	}

	protected static class ReportRendering extends ARenderer.ARendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> {
		protected IReportRenderContext appendLevel(IFinding finding, IReportRenderContext context) {
			if (context.getMode().compareTo(ExplanationMode.Describe) >= 0) context.append(finding.getLevel()).append(": ");
			return context;
		}

		protected IReportRenderContext explain(IContractTermFinding finding, IReportRenderContext context) {
			if (context.getMode().compareTo(ExplanationMode.Describe) >= 0) {
				if ((finding.getLevel().compareTo(Level.WARN) <= 0) || (context.getMode().compareTo(ExplanationMode.Explain) >= 0)) try (final ICloseable indent = context.newline().indent()) {
					final ExpressionContextFinding findingContext = context.getFindingContext();
					if (findingContext != null) context.append("Rule: ").render(findingContext.getExpression(), IExpression.class).newline();
					context.append("Explanation: ").render(finding.getResult());
				}
			}
			return context;
		}

		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			builder.add(IExplicitReportRenderable.class, e -> c -> ((IExplicitReportRenderable<?>) e).render(c));
			builder.add(IExplained.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final IExplained<TermRelation> expression = e;
				c.render(expression);
			});

			builder.add(IReport.class, e -> c -> {
				c.append("Minimum finding level: ");
				final Collection<IFinding> findings = e.getFindings();
				if (findings.isEmpty()) c.append("NONE").newline();
				else {
					c.append(e.getMinLevel()).newline();
					if (findings != null) for (IFinding finding : findings) {
						c.render(finding, IFinding.class).newline();
					}
				}
			});

			builder.add(ILicenseTerm.class, e -> c -> c.append('\"').append(e.getDescription()).append('\"'));
			builder.add(IUsageTerm.class, e -> c -> c.append('\"').append(e.getDescription()).append('\"'));

			builder.add(ILiteral.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final ILiteral<IContractComparisonName, TermRelation> expression = e;
				c.append('(').name(expression.getName()).append(')');
			});
			builder.add(IVariable.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final IVariable<IContractComparisonName, TermRelation> expression = e;
				c.render(expression.getName().getTerm(), ITerm.class);
			});
			builder.add(IOperation.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final IOperation<IContractComparisonName, TermRelation> castE = (IOperation<IContractComparisonName, TermRelation>) e;

				c.append('(');
				final String separator;
				final BooleanOperation.Operator operator = (BooleanOperation.Operator) castE.getOperator();
				switch (operator) {
					case NOT:
						c.append('!').render(HCollection.getOne(castE.getArguments()), IExpression.class).append(')');
						return;
					case AND:
						separator = "&&";
						break;
					case OR:
						separator = "||";
						break;
					case XOR:
						separator = "^";
						break;
					default:
						throw new EnumException(BooleanOperation.Operator.class, operator);
				}

				boolean first = true;
				for (IExpression<IContractComparisonName, TermRelation> argument : castE.getArguments()) {
					if (first) first = false;
					else c.append(' ').append(separator).append(' ');
					c.render(argument, IExpression.class);
				}
				c.append(')');
			});

			builder.add(IContextFinding.class, e -> c -> c.render(e.getFinding(), IFinding.class));
			builder.add(ExpressionContextFinding.class, e -> c -> {
				try (final ICloseable findingContext = c.findingContext(e)) {
					c.render(e.getFinding(), IFinding.class);
				}
			});

			builder.add(IncompatibleWorkLicenseFinding.class, e -> c -> {
				appendLevel(e, c).append("A license is incompatible with a combined work that includes artifacts under this license");
				if (c.getMode().compareTo(ExplanationMode.Describe) >= 0) try (final ICloseable indent = c.indent()) {
					if ((e.getUnknown() != null) && !e.getUnknown().isEmpty()) {
						c.newline().append("License terms with unknown compatability: ");
						final List<ILicenseTerm> unknown = new ArrayList<>(e.getUnknown());
						final int size = unknown.size();
						for (int i = 0; i < size; i++) {
							if (i > 0) c.append((i == size - 1) ? " & " : ", ");
							c.render(unknown.get(i), ILicenseTerm.class);
						}
					}
					if ((e.getMismatched() != null) && !e.getMismatched().isEmpty()) {
						c.newline().append("Mistmatched license terms: ");
						final List<ILicenseTerm> mistmatched = new ArrayList<>(e.getMismatched());
						final int size = mistmatched.size();
						for (int i = 0; i < size; i++) {
							if (i > 0) c.append((i == size - 1) ? " & " : ", ");
							c.render(mistmatched.get(i), ILicenseTerm.class);
						}
					}
				}
			});
			builder.add(MultiLicenseFinding.class, e -> c -> appendLevel(e, c).append("Multiple, conflicting licenses detected for artifact"));
			builder.add(MultiUsageFinding.class, e -> c -> appendLevel(e, c).append("Multiple, conflicting usages detected for artifact"));
			builder.add(UnknownWorkFinding.class, e -> c -> {
				appendLevel(e, c).append("Unknown work type");
				if (c.getMode().compareTo(ExplanationMode.Describe) >= 0) try (final ICloseable indent = c.indent()) {
					c.newline().append((c.getMode().compareTo(ExplanationMode.Trace) >= 0) ? HError.toString(e.getThrowable()) : e.getThrowable().getMessage());
				}
			});
			builder.add(UnrecognizedTermFinding.class, e -> c -> {
				final ContractType type = ContractType.valueOf(e.getTerm());
				appendLevel(e, c).render(e.getTerm(), ITerm.class).append(" is not recognized so we cannot analyze ");
				switch (type) {
					case License:
						c.append("whether the condition has been met.");
						break;
					case Usage:
						c.append("whether the licenses allow this usage.");
						break;
					default:
						throw new EnumException(ContractType.class, type);
				}
			});
			builder.add(IConditionFinding.class, e -> c -> {
				final ExpressionContextFinding findingContext = c.getFindingContext();
				final Collection<ITerm> outputs = findingContext == null ? HCollection.emptyList() : findingContext.getOutputs();

				appendLevel(e, c).append("Condition");
				if (outputs.size() > 1) c.append('s');
				c.append(' ');
				if (!outputs.isEmpty()) {
					final List<ITerm> list = new ArrayList<>(outputs);
					final int size = list.size();
					for (int i = 0; i < size; i++) {
						if (i > 0) c.append((i == size - 1) ? " & " : ", ");
						c.render(list.get(i), ITerm.class);
					}
					c.append(' ');
				}

				c.append(outputs.size() > 1 ? "are" : "is");
				if (!e.isSatisfied()) c.append(" not");
				c.append(" satisfied");

				if (findingContext != null) {
					c.append(" based on ");
					final List<ITerm> list = new ArrayList<>(findingContext.getInputs());
					final int size = list.size();
					for (int i = 0; i < size; i++) {
						if (i > 0) c.append((i == size - 1) ? " & " : ", ");
						c.render(list.get(i), ITerm.class);
					}
				}

				explain(e, c);
			});
			builder.add(DiscloseSourceFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must disclose the source for this artifact")));
			builder.add(CopyrightNoticeFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must publish a copyright and license notice stating that you use this artifact")));
			builder.add(StateChangesFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must state the changes you have made to your copy of this artifact")));
			builder.add(SuspiciousUsageFinding.class, e -> c -> explain(e, appendLevel(e, c).append("The usage for this artifact improperly specifies the ").append(e.getAttribute().getDescription())));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new ReportRendering();

	protected final ExplanationRenderer<IContractComparisonName, TermRelation> explanationRenderer;

	public ReportRenderer(ExplanationMode mode, IContext context) {
		this.explanationRenderer = new ExplanationRenderer<>(mode, new ContractComparisonNameRenderer(context), TermRelationValueSystem.create(), TermRelationOperationSystem.create());
	}

	@Override
	protected ReportRenderContext createContext(TextNestedModified.TextNestedModifiedBuilder builder) {
		return new ReportRenderContext(builder, getExplanationRenderer());
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
