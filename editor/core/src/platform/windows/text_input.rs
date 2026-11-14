use core::pin::Pin;

use windows::{
    Foundation::TypedEventHandler,
    UI::Text::Core::{
        CoreTextCompositionCompletedEventArgs, CoreTextCompositionStartedEventArgs,
        CoreTextEditContext, CoreTextFormatUpdatingEventArgs, CoreTextLayoutRequest,
        CoreTextLayoutRequestedEventArgs, CoreTextRange, CoreTextSelectionRequest,
        CoreTextSelectionRequestedEventArgs, CoreTextSelectionUpdatingEventArgs,
        CoreTextTextRequestedEventArgs, CoreTextTextUpdatingEventArgs,
    },
};
use windows_core::{HSTRING, IInspectable};

use crate::{
    CoreLoop, SystemLink,
    input::hittest::{HitTestTreeManager, HitTestTreeRef},
    rendering::text::FontSet,
    uicore::{ViewIdentifier, ViewRenderQueue, ViewRenderer},
};

pub struct NativeTextInputContextCreateContext {
    pub coreloop: *mut CoreLoop<'static>,
}

pub struct CoreTextLayoutContext<'env> {
    pub ht_manager: &'env HitTestTreeManager,
    pub font_set: &'env FontSet,
}

pub struct CoreTextUpdateContext<'env> {
    pub view_render_queue: &'env mut ViewRenderQueue,
}
impl<'env> ViewRenderer for CoreTextUpdateContext<'env> {
    #[inline(always)]
    fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
        self.view_render_queue.schedule(target)
    }
}

pub trait TextProvider {
    fn text(&self, range: CoreTextRange) -> windows_core::Result<HSTRING>;
    fn selection(&self, req: &CoreTextSelectionRequest) -> windows_core::Result<()>;
}
pub trait CoreTextDeferrableEventHandler {
    fn layout(
        &self,
        ctx: &CoreTextLayoutContext,
        req: &CoreTextLayoutRequest,
    ) -> windows_core::Result<()>;
    fn text_updating(
        &self,
        ctx: &mut CoreTextUpdateContext,
        e: &CoreTextTextUpdatingEventArgs,
    ) -> windows_core::Result<()>;
    fn format_updating<'x>(
        &'x self,
        ctx: &mut CoreTextUpdateContext,
        e: &CoreTextFormatUpdatingEventArgs,
    ) -> windows_core::Result<()>;
}
pub struct NativeTextInputContext<T: TextProvider + 'static> {
    edit_context: CoreTextEditContext,
    _text_provider: Pin<Box<std::rc::Weak<T>>>,
}
impl<T: TextProvider + 'static> NativeTextInputContext<T> {
    pub fn new(
        system_link: &SystemLink,
        layout_provider_ht: HitTestTreeRef,
        text_provider: &std::rc::Weak<T>,
    ) -> Self {
        let edit_context = system_link
            .app_context
            .ctm
            .CreateEditContext()
            .expect("CoreTextServicesManager.CreateEditContext");
        let text_provider = Box::pin(text_provider.clone());
        let text_provider_ptr = core::ptr::from_ref(text_provider.as_ref().get_ref());

        // TODO: CoreTextとRustの相性がありえん悪いのをなんかいい感じになんとかしたい
        let caller_thread_id = std::thread::current().id();
        edit_context
            .LayoutRequested(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextLayoutRequestedEventArgs,
            >::new({
                let coreloop = std::sync::atomic::AtomicPtr::new(
                    system_link
                        .native_text_input_context_create_context
                        .coreloop,
                );
                move |_sender, e| {
                    let e = e.ok().expect("event_args.null");
                    let req = e.Request().expect("layout_requested.event_args.request");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );
                    unsafe { &**coreloop.as_ptr() }
                        .core_text_compute_layout(req, layout_provider_ht)
                }
            }))
            .expect("edit_context.layout_requested");
        edit_context
            .TextRequested(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextTextRequestedEventArgs,
            >::new({
                let text_provider_ptr =
                    std::sync::atomic::AtomicPtr::new(text_provider_ptr.cast_mut());
                move |_sender, e| {
                    let e = e.ok().expect("event_args.null");
                    let req = e.Request().expect("text_requested.event_args.request");
                    tracing::trace!(
                        req.range = ?req.Range(),
                        "edit_context.text_requested"
                    );

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    let Some(text_provider) = unsafe { &**text_provider_ptr.as_ptr() }.upgrade()
                    else {
                        tracing::warn!("text provider has defunct!");
                        return Ok(());
                    };
                    req.SetText(&text_provider.text(req.Range()?)?)?;
                    Ok(())
                }
            }))
            .expect("edit_context.text_requested");
        edit_context
            .TextUpdating(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextTextUpdatingEventArgs,
            >::new({
                let coreloop = std::sync::atomic::AtomicPtr::new(
                    system_link
                        .native_text_input_context_create_context
                        .coreloop,
                );
                move |_sender, e| {
                    let e = e.cloned().expect("event_args.null");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    unsafe { Pin::new_unchecked(&mut **coreloop.as_ptr()) }
                        .core_text_update_text(e, layout_provider_ht)
                }
            }))
            .expect("edit_context.text_updating");
        edit_context
            .CompositionStarted(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextCompositionStartedEventArgs,
            >::new(|_sender, e| {
                let _e = e.ok().expect("event_args.null");
                tracing::trace!("edit_context.composition_started");
                Ok(())
            }))
            .expect("edit_context.composition_started");
        edit_context
            .CompositionCompleted(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextCompositionCompletedEventArgs,
            >::new(move |_sender, _e| {
                // let e = e.ok().expect("event_args.null");
                tracing::trace!(
                    // composition_segments = ?e.CompositionSegments(),
                    // composition_segments.len = ?e.CompositionSegments().and_then(|x| x.Size()),
                    "edit_context.composition_completed"
                );

                /*for segment in e.CompositionSegments().expect("edit_context.composition_copmleted.composition_segments") {
                    tracing::trace!(
                        preconversion_string = ?segment.PreconversionString().map(|x| x.to_string_lossy()),
                        range = ?segment.Range(),
                        "edit_context.composition_completed.segment"
                    );
                }*/

                Ok(())
            }))
            .expect("edit_context.composition_completed");
        edit_context
            .FormatUpdating(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextFormatUpdatingEventArgs,
            >::new({
                let coreloop = std::sync::atomic::AtomicPtr::new(
                    system_link
                        .native_text_input_context_create_context
                        .coreloop,
                );
                move |_sender, e| {
                    let e = e.cloned().expect("event_args.null");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    unsafe { Pin::new_unchecked(&mut **coreloop.as_ptr()) }
                        .core_text_update_format(e, layout_provider_ht)
                }
            }))
            .expect("edit_context.format_updating");
        edit_context
            .FocusRemoved(&TypedEventHandler::<
                CoreTextEditContext,
                windows_core::IInspectable,
            >::new(|_sender, e| {
                tracing::trace!(e = ?e.ok(), "edit_context.focus_removed");

                Ok(())
            }))
            .expect("edit_context.focus_removed");
        edit_context
            .NotifyFocusLeaveCompleted(
                &TypedEventHandler::<CoreTextEditContext, IInspectable>::new(|_sender, e| {
                    tracing::trace!(e = ?e.ok(), "edit_context.notify_focus_leave_completed");

                    Ok(())
                }),
            )
            .expect("edit_context.notify_focus_leave_completed");
        edit_context
            .SelectionRequested(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextSelectionRequestedEventArgs,
            >::new({
                let text_provider_ptr =
                    std::sync::atomic::AtomicPtr::new(text_provider_ptr.cast_mut());
                move |_sender, e| {
                    let e = e.ok().expect("event_args.null");
                    let req = e
                        .Request()
                        .expect("edit_context.selection_requested.event_args.request");
                    tracing::trace!("edit_context.selection_requested");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    let Some(text_provider) = unsafe { &**text_provider_ptr.as_ptr() }.upgrade()
                    else {
                        tracing::warn!("text provider has defunct!");
                        return Ok(());
                    };
                    text_provider.selection(&req)?;
                    Ok(())
                }
            }))
            .expect("edit_context.selection_requested");
        edit_context
            .SelectionUpdating(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextSelectionUpdatingEventArgs,
            >::new(|_sender, e| {
                let e = e.ok().expect("event_args.null");
                tracing::trace!(
                    // is_canceled = ?e.IsCanceled(),
                    selection = ?e.Selection(),
                    "edit_context.selection_updating"
                );

                Ok(())
            }))
            .expect("edit_context.selection_updating");

        Self {
            edit_context,
            _text_provider: text_provider,
        }
    }

    pub fn notify_focus_enter(&self) {
        self.edit_context
            .NotifyFocusEnter()
            .expect("edit_context.NotifyFocusEnter");
    }

    pub fn notify_focus_leave(&self) {
        self.edit_context
            .NotifyFocusLeave()
            .expect("edit_context.NotifyFocusLeave");
    }

    pub fn notify_layout_changed(&self) {
        self.edit_context
            .NotifyLayoutChanged()
            .expect("edit_context.NotifyLayoutChanged");
    }

    pub fn notify_selection_changed(&self, start_acp: i32, end_acp: i32) {
        self.edit_context
            .NotifySelectionChanged(CoreTextRange {
                StartCaretPosition: start_acp,
                EndCaretPosition: end_acp,
            })
            .expect("edit_context.NotifySelectionChanged");
    }
}
