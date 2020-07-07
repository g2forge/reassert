package com.g2forge.reassert.term.analyze.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

public interface IExplicitReportRenderable<T> extends IExplicitRenderable<IReportRenderContext>, IExplained<T> {}
