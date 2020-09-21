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
import com.g2forge.enigma.backend.convert.textual.ISimpleTextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ToStringTextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified.TextNestedModifiedBuilder;
import com.g2forge.reassert.contract.model.IExpressionContext;
import com.g2forge.reassert.contract.model.TermConstant;
import com.g2forge.reassert.contract.model.TermType;
import com.g2forge.reassert.contract.model.findings.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.findings.UnrecognizedTermFinding;
import com.g2forge.reassert.contract.model.findings.rule.ConditionFinding;
import com.g2forge.reassert.contract.model.findings.rule.DiscloseSourceFinding;
import com.g2forge.reassert.contract.model.findings.rule.IRuleFinding;
import com.g2forge.reassert.contract.model.findings.rule.NoticeFinding;
import com.g2forge.reassert.contract.model.findings.rule.StateChangesFinding;
import com.g2forge.reassert.contract.model.findings.rule.SuspiciousUsageFinding;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.MultiLicenseFinding;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.MultiUsageFinding;
import com.g2forge.reassert.core.model.report.IContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.work.IncompatibleWorkLicenseFinding;
import com.g2forge.reassert.core.model.work.UnknownWorkTypeFinding;
import com.g2forge.reassert.express.explain.convert.ExplanationMode;
import com.g2forge.reassert.express.explain.convert.ExplanationRenderer;
import com.g2forge.reassert.express.explain.model.IExplained;
import com.g2forge.reassert.express.express.IConstant;
import com.g2forge.reassert.express.express.IExpression;
import com.g2forge.reassert.express.express.Operation;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReportRenderer extends ATextualRenderer<Object, IReportRenderContext> {
	@Getter(AccessLevel.PROTECTED)
	@RequiredArgsConstructor
	protected static class NameRenderer implements ISimpleTextualRenderer<Object> {
		protected final IContext context;

		@Override
		public void render(TextNestedModifiedBuilder builder, Object renderable) {
			if (renderable instanceof TermConstant) {
				final TermConstant constant = (TermConstant) renderable;

				builder.expression(constant.getTerm().getDescription()).expression(" in ");

				final IDescription description = getContext().describe(constant.getContract());
				builder.expression(description.getName());
			} else builder.expression(renderable);
		}
	}

	@Getter(AccessLevel.PROTECTED)
	protected class ReportRenderContext extends ARenderContext implements IReportRenderContext {
		@Getter(AccessLevel.PUBLIC)
		protected final ExplanationMode mode;

		protected final ExplanationRenderer explanationRenderer;

		@Getter(AccessLevel.PUBLIC)
		protected IExpressionContext findingContext;

		public ReportRenderContext(TextNestedModified.TextNestedModifiedBuilder builder, ExplanationMode mode, ITextualRenderer<Object> valueRenderer, ITextualRenderer<Object> nameRenderer) {
			super(builder);
			this.mode = mode;
			this.explanationRenderer = new ExplanationRenderer(getMode(), valueRenderer, nameRenderer);
		}

		@Override
		public ICloseable findingContext(IExpressionContext findingContext) {
			final IExpressionContext previous = this.findingContext;
			this.findingContext = findingContext;
			return () -> this.findingContext = previous;
		}

		@Override
		protected IReportRenderContext getThis() {
			return this;
		}

		@Override
		public IReportRenderContext render(IExplained<?> explained) {
			explanationRenderer.render(getBuilder(), explained);
			return this;
		}
	}

	protected static class ReportRendering extends ARenderer.ARendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> {
		protected IReportRenderContext appendLevel(IFinding finding, IReportRenderContext context) {
			if (context.getMode().compareTo(ExplanationMode.Describe) >= 0) context.append(finding.getLevel()).append(": ");
			return context;
		}

		protected IReportRenderContext explain(IRuleFinding finding, IReportRenderContext context) {
			if (context.getMode().compareTo(ExplanationMode.Describe) >= 0) {
				if ((finding.getLevel().compareTo(Level.WARN) <= 0) || (context.getMode().compareTo(ExplanationMode.Explain) >= 0)) try (final ICloseable indent = context.newline().indent()) {
					final IExpressionContext findingContext = context.getFindingContext();
					if (findingContext != null) context.append("Rule: ").render(findingContext.getExpression(), IExpression.class).newline();
					context.append("Explanation: ").render(finding.getResult());
				}
			}
			return context;
		}

		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			builder.add(IExplicitReportRenderable.class, e -> c -> ((IExplicitReportRenderable<?>) e).render(c));
			builder.add(IExplained.class, e -> c -> c.render(e));

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

			builder.add(IConstant.class, e -> c -> c.append('(').append(e.getName()).append(')'));
			builder.add(TermConstant.class, e -> c -> c.render(e.getTerm(), ITerm.class));
			builder.add(Operation.class, e -> c -> {
				final Operation<?> castE = (Operation<?>) e;
				c.append('(');
				final String separator;
				switch (castE.getOperator()) {
					case NOT:
						c.append('!').render(HCollection.getOne(castE.getArguments()), IExpression.class).append(')');
						return;
					case AND:
						separator = "&&";
						break;
					case OR:
						separator = "||";
						break;
					default:
						throw new EnumException(Operation.Operator.class, castE.getOperator());
				}

				boolean first = true;
				for (IExpression<?> argument : castE.getArguments()) {
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
			builder.add(UnknownWorkTypeFinding.class, e -> c -> {
				appendLevel(e, c).append("Unknown work type");
				if (c.getMode().compareTo(ExplanationMode.Describe) >= 0) try (final ICloseable indent = c.indent()) {
					c.newline().append((c.getMode().compareTo(ExplanationMode.Trace) >= 0) ? HError.toString(e.getThrowable()) : e.getThrowable().getMessage());
				}
			});
			builder.add(UnrecognizedTermFinding.class, e -> c -> {
				final TermType termType = TermType.valueOf(e.getTerm());
				appendLevel(e, c).render(e.getTerm(), ITerm.class).append(" is not recognized so we cannot analyze ");
				switch (termType) {
					case License:
						c.append("whether the condition has been met.");
						break;
					case Usage:
						c.append("whether the licenses allow this usage.");
						break;
					default:
						throw new EnumException(TermType.class, termType);
				}
			});
			builder.add(ConditionFinding.class, e -> c -> {
				final IExpressionContext findingContext = c.getFindingContext();
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
			builder.add(NoticeFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must publish a copyright and license notice stating that you use this artifact")));
			builder.add(StateChangesFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must state the changes you have made to your copy of this artifact")));
			builder.add(SuspiciousUsageFinding.class, e -> c -> explain(e, appendLevel(e, c).append("The usage for this artifact improperly specifies the ").append(e.getAttribute().getDescription())));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new ReportRendering();

	protected final ExplanationMode mode;

	protected final ITextualRenderer<Object> valueRenderer;

	protected final ITextualRenderer<Object> nameRenderer;

	public ReportRenderer(ExplanationMode mode, IContext context) {
		this(mode, ToStringTextualRenderer.create(), new NameRenderer(context));
	}

	public ReportRenderer(IContext context) {
		this(ExplanationMode.Explain, context);
	}

	@Override
	protected ReportRenderContext createContext(TextNestedModified.TextNestedModifiedBuilder builder) {
		return new ReportRenderContext(builder, getMode(), getValueRenderer(), getNameRenderer());
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
